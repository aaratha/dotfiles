#!/usr/bin/env bash
#
# patch-exanima-macos.sh
# ----------------------------------------------------------------------------
# Make Exanima render natively under CrossOver/Wine on Apple Silicon (and Intel)
# macOS, using Apple's OpenGL
#
# WHAT IT DOES (idempotent — safe to re-run, e.g. after a CrossOver update):
#   1. Builds an OpenGL.framework "facade" dylib at /Users/Shared/rgtc_gl.dylib
#      that re-exports real OpenGL and adds core-profile compatibility shims:
#        - RGTC/BC4/BC5 + LATC texture software decompression
#        - GL_ALPHA / GL_LUMINANCE textures via R8/RG8 + swizzle (fonts, UI)
#        - strips the stray ';' Apple's GLSL compiler rejects after functions
#        - emulates the default VAO 0 (illegal in core profile)
#        - translates client-side vertex/index arrays into VBOs
#        - advertises RGTC so the renderer starts
#   2. Patches winemac.so (pattern-based, version-robust):
#        - forces the core GL profile (r12d=3)
#        - forces core-profile attributes into the pixel format (jb -> nop)
#        - redirects its OpenGL dlopen to the facade
#        - strips RTLD_NOLOAD so the facade actually loads
#      then ad-hoc re-signs it.
#   3. Patches opengl32.dll (PE-side, CrossOver Preview only):
#        - bypasses the wglGetProcAddress version check that blocks GL 3.0+
#          functions when the cached context version is 2.1
#
# AFTER RUNNING — two manual steps:
#   * In CrossOver, set the bottle's RetinaMode to OFF (Wine registry
#     HKCU\Software\Wine\Mac Driver\RetinaMode = "n"), or the UI is mis-scaled.
#   * Launch Exanima normally through Steam/CrossOver — no wrapper needed.
#
# REQUIREMENTS: Xcode CLT (clang, codesign), python3.
# Tested with CrossOver 26.1 and CrossOver Preview 27.0.
# If a CrossOver update overwrites winemac.so/opengl32.dll — just re-run.
# ----------------------------------------------------------------------------
set -euo pipefail

DYLIB="/Users/Shared/rgtc_gl.dylib"   # MUST stay <= 50 chars (replaces the
                                      # OpenGL.framework path inside winemac.so)

say(){ printf '\033[1;36m==>\033[0m %s\n' "$*"; }
die(){ printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

# Locate the CrossOver install in /Applications or ~/Applications.
# Override with CROSSOVER_APP=/path/to/CrossOver.app if installed elsewhere.
wm_rel="Contents/SharedSupport/CrossOver/lib/wine/x86_64-unix/winemac.so"
if [ -z "${CROSSOVER_APP:-}" ]; then
    for cand in \
        "/Applications/CrossOver.app" \
        "/Applications/CrossOver Preview.app" \
        "$HOME/Applications/CrossOver.app" \
        "$HOME/Applications/CrossOver Preview.app"; do
        if [ -f "$cand/$wm_rel" ]; then CROSSOVER_APP="$cand"; break; fi
    done
fi
[ -n "${CROSSOVER_APP:-}" ] || die "CrossOver not found in /Applications or ~/Applications. Set CROSSOVER_APP=/path/to/CrossOver.app"
WINE_UNIX="$CROSSOVER_APP/Contents/SharedSupport/CrossOver/lib/wine/x86_64-unix"
WINEMAC="$WINE_UNIX/winemac.so"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
say "Using CrossOver at: $CROSSOVER_APP"

# --- preflight ---------------------------------------------------------------
command -v clang   >/dev/null || die "clang not found. Run: xcode-select --install"
command -v codesign>/dev/null || die "codesign not found (install Xcode CLT)."
command -v python3 >/dev/null || die "python3 not found."
[ -f "$WINEMAC" ] || die "winemac.so not found at $WINEMAC (set CROSSOVER_APP=...)."
[ "${#DYLIB}" -le 50 ] || die "DYLIB path longer than 50 bytes; won't fit the patch slot."

# --- 1. build the facade dylib ----------------------------------------------
say "Writing facade source"
cat > "$TMP/rgtc_gl.c" <<'RGTC_GL_SOURCE_EOF'
/*
 * rgtc_gl.c — OpenGL.framework compatibility facade for running Exanima
 * (and similar legacy-GL games) under CrossOver/Wine on macOS.
 *
 * Apple's macOS OpenGL only offers a GL 4.1 *core* profile for modern features
 * (UBOs, GLSL 1.50+), but core profile removes a pile of things this game
 * relies on. Wine's macdrv resolves GL entry points with
 *     handle = dlopen("/System/Library/Frameworks/OpenGL.framework/OpenGL");
 *     fn     = dlsym(handle, "glFoo");
 * so DYLD interposing can't touch them. Instead winemac.so is patched to
 * dlopen THIS dylib. We re-export all of OpenGL.framework (so the ~2000
 * ordinary GL functions resolve normally) and override just the entry points
 * that need compatibility shims:
 *
 *   1. RGTC/BC4/BC5 + LATC textures      -> software-decompress to R8/RG8
 *   2. GL_ALPHA / GL_LUMINANCE textures  -> R8/RG8 + texture swizzle
 *   3. stray ';' after GLSL functions    -> rewritten in glShaderSource
 *   4. default VAO 0 (illegal in core)   -> a real VAO kept transparently bound
 *   5. client-side vertex/index arrays   -> uploaded to scratch VBOs per draw
 *   6. RGTC capability queries           -> advertised via glGetString / glGetIntegerv
 *
 * Set RGTC_GL_LOG=1 for diagnostics (shader compile/link errors, etc.).
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <unistd.h>

typedef unsigned int  GLenum;
typedef int           GLint;
typedef int           GLsizei;
typedef unsigned int  GLuint;
typedef unsigned char GLubyte;

/* enums */
#define GL_BYTE 0x1400
#define GL_UNSIGNED_BYTE 0x1401
#define GL_RED 0x1903
#define GL_GREEN 0x1904
#define GL_RG 0x8227
#define GL_R8 0x8229
#define GL_RG8 0x822B
#define GL_R8_SNORM 0x8F94
#define GL_RG8_SNORM 0x8F95
#define GL_EXTENSIONS 0x1F03
#define GL_NUM_EXTENSIONS 0x821D
#define GL_NUM_COMPRESSED_TEXTURE_FORMATS 0x86A2
#define GL_COMPRESSED_TEXTURE_FORMATS 0x86A3
#define GL_ARRAY_BUFFER 0x8892
#define GL_ELEMENT_ARRAY_BUFFER 0x8893
#define GL_STREAM_DRAW 0x88E0
#define GL_COMPILE_STATUS 0x8B81
#define GL_LINK_STATUS 0x8B82
#define GL_TEXTURE_SWIZZLE_R 0x8E42
#define GL_TEXTURE_SWIZZLE_G 0x8E43
#define GL_TEXTURE_SWIZZLE_B 0x8E44
#define GL_TEXTURE_SWIZZLE_A 0x8E45
#define GL_COMPRESSED_RED_RGTC1 0x8DBB
#define GL_COMPRESSED_SIGNED_RED_RGTC1 0x8DBC
#define GL_COMPRESSED_RG_RGTC2 0x8DBD
#define GL_COMPRESSED_SIGNED_RG_RGTC2 0x8DBE
#define GL_COMPRESSED_LUMINANCE_LATC1_EXT 0x8C70
#define GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT 0x8C71
#define GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT 0x8C72
#define GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT 0x8C73

static const GLint kRgtcFormats[] = {
    GL_COMPRESSED_RED_RGTC1, GL_COMPRESSED_SIGNED_RED_RGTC1,
    GL_COMPRESSED_RG_RGTC2,  GL_COMPRESSED_SIGNED_RG_RGTC2,
    GL_COMPRESSED_LUMINANCE_LATC1_EXT, GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT,
    GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT, GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT,
};
#define kRgtcFormatCount ((GLint)(sizeof(kRgtcFormats)/sizeof(kRgtcFormats[0])))
static const char* kInjectedExts[] = {
    "GL_ARB_texture_compression_rgtc", "GL_EXT_texture_compression_rgtc",
    "GL_EXT_texture_compression_latc",
};
#define kInjectedExtCount 3

/* real OpenGL.framework entry points */
static void *g_gl;
static void (*r_TexImage2D)(GLenum,GLint,GLint,GLsizei,GLsizei,GLint,GLenum,GLenum,const void*);
static void (*r_TexImage3D)(GLenum,GLint,GLint,GLsizei,GLsizei,GLsizei,GLint,GLenum,GLenum,const void*);
static void (*r_TexSubImage2D)(GLenum,GLint,GLint,GLint,GLsizei,GLsizei,GLenum,GLenum,const void*);
static void (*r_TexSubImage3D)(GLenum,GLint,GLint,GLint,GLint,GLsizei,GLsizei,GLsizei,GLenum,GLenum,const void*);
static void (*r_GetIntegerv)(GLenum,GLint*);
static const GLubyte* (*r_GetString)(GLenum);
static const GLubyte* (*r_GetStringi)(GLenum,GLuint);
static void (*r_GenBuffers)(GLsizei,GLuint*);
static void (*r_BufferData)(GLenum,long,const void*,GLenum);
static void (*r_BindBuffer)(GLenum,GLuint);
static void (*r_TexParameteri)(GLenum,GLenum,GLint);
static void (*r_GenVertexArrays)(GLsizei,GLuint*);
static void (*r_BindVertexArray)(GLuint);
static void (*r_VertexAttribPointer)(GLuint,GLint,GLenum,unsigned char,GLsizei,const void*);
static void (*r_VertexAttribIPointer)(GLuint,GLint,GLenum,GLsizei,const void*);
static void (*r_EnableVertexAttribArray)(GLuint);
static void (*r_DisableVertexAttribArray)(GLuint);
static void (*r_DrawArrays)(GLenum,GLint,GLsizei);
static void (*r_DrawElements)(GLenum,GLsizei,GLenum,const void*);
static void (*r_DrawRangeElements)(GLenum,GLuint,GLuint,GLsizei,GLenum,const void*);
static void (*r_DrawElementsInstanced)(GLenum,GLsizei,GLenum,const void*,GLsizei);
static void (*r_DrawArraysInstanced)(GLenum,GLint,GLsizei,GLsizei);
static void (*r_ShaderSource)(GLuint,GLsizei,const char* const*,const GLint*);
/* diagnostics-only (gated) */
static void (*r_GetShaderiv)(GLuint,GLenum,GLint*);
static void (*r_GetShaderInfoLog)(GLuint,GLsizei,GLsizei*,char*);
static void (*r_GetProgramiv)(GLuint,GLenum,GLint*);
static void (*r_GetProgramInfoLog)(GLuint,GLsizei,GLsizei*,char*);

static int g_log = -1;
static void rlog(const char* fmt, ...){
    if(g_log<0){ const char* e=getenv("RGTC_GL_LOG"); g_log=(e&&*e&&*e!='0')?1:0; }
    if(!g_log) return;
    va_list ap; va_start(ap,fmt); fprintf(stderr,"[rgtc_gl] "); vfprintf(stderr,fmt,ap);
    fputc('\n',stderr); va_end(ap);
}

__attribute__((constructor))
static void ctor(void){
    g_gl = dlopen("/System/Library/Frameworks/OpenGL.framework/Versions/A/OpenGL", RTLD_NOW|RTLD_LOCAL);
    if(!g_gl){ fprintf(stderr,"[rgtc_gl] FATAL: dlopen OpenGL: %s\n", dlerror()); return; }
    #define L(n) dlsym(g_gl, n)
    r_TexImage2D=L("glTexImage2D"); r_TexImage3D=L("glTexImage3D");
    r_TexSubImage2D=L("glTexSubImage2D"); r_TexSubImage3D=L("glTexSubImage3D");
    r_GetIntegerv=L("glGetIntegerv"); r_GetString=L("glGetString"); r_GetStringi=L("glGetStringi");
    r_GenBuffers=L("glGenBuffers"); r_BufferData=L("glBufferData"); r_BindBuffer=L("glBindBuffer");
    r_TexParameteri=L("glTexParameteri");
    r_GenVertexArrays=L("glGenVertexArrays"); r_BindVertexArray=L("glBindVertexArray");
    r_VertexAttribPointer=L("glVertexAttribPointer"); r_VertexAttribIPointer=L("glVertexAttribIPointer");
    r_EnableVertexAttribArray=L("glEnableVertexAttribArray"); r_DisableVertexAttribArray=L("glDisableVertexAttribArray");
    r_DrawArrays=L("glDrawArrays"); r_DrawElements=L("glDrawElements");
    r_DrawRangeElements=L("glDrawRangeElements"); r_DrawElementsInstanced=L("glDrawElementsInstanced");
    r_DrawArraysInstanced=L("glDrawArraysInstanced"); r_ShaderSource=L("glShaderSource");
    r_GetShaderiv=L("glGetShaderiv"); r_GetShaderInfoLog=L("glGetShaderInfoLog");
    r_GetProgramiv=L("glGetProgramiv"); r_GetProgramInfoLog=L("glGetProgramInfoLog");
    #undef L
    rlog("facade loaded");
}

/* ---------- RGTC / BC4 / BC5 decode ---------- */
static void bc4u(const uint8_t* b,uint8_t* o,size_t st,int ch,int wc,int hc){
    uint8_t r0=b[0],r1=b[1],p[8]; p[0]=r0; p[1]=r1;
    if(r0>r1){p[2]=(6*r0+r1)/7;p[3]=(5*r0+2*r1)/7;p[4]=(4*r0+3*r1)/7;p[5]=(3*r0+4*r1)/7;p[6]=(2*r0+5*r1)/7;p[7]=(r0+6*r1)/7;}
    else{p[2]=(4*r0+r1)/5;p[3]=(3*r0+2*r1)/5;p[4]=(2*r0+3*r1)/5;p[5]=(r0+4*r1)/5;p[6]=0;p[7]=255;}
    uint64_t ix=0; for(int i=0;i<6;i++) ix|=((uint64_t)b[2+i])<<(i*8);
    for(int y=0;y<4;y++){if(y>=hc)break;for(int x=0;x<4;x++){if(x>=wc)break;o[y*st+x*ch]=p[(ix>>(3*(y*4+x)))&7];}}
}
static void bc4s(const uint8_t* b,int8_t* o,size_t st,int ch,int wc,int hc){
    int8_t r0=(int8_t)b[0],r1=(int8_t)b[1],p[8]; if(r0==-128)r0=-127; if(r1==-128)r1=-127; p[0]=r0;p[1]=r1;
    if(r0>r1){p[2]=(6*r0+r1)/7;p[3]=(5*r0+2*r1)/7;p[4]=(4*r0+3*r1)/7;p[5]=(3*r0+4*r1)/7;p[6]=(2*r0+5*r1)/7;p[7]=(r0+6*r1)/7;}
    else{p[2]=(4*r0+r1)/5;p[3]=(3*r0+2*r1)/5;p[4]=(2*r0+3*r1)/5;p[5]=(r0+4*r1)/5;p[6]=-127;p[7]=127;}
    uint64_t ix=0; for(int i=0;i<6;i++) ix|=((uint64_t)b[2+i])<<(i*8);
    for(int y=0;y<4;y++){if(y>=hc)break;for(int x=0;x<4;x++){if(x>=wc)break;o[y*st+x*ch]=p[(ix>>(3*(y*4+x)))&7];}}
}
static void bc4_img(const uint8_t* s,void* d,GLsizei w,GLsizei h,int sg){
    int bw=(w+3)/4,bh=(h+3)/4;
    for(int by=0;by<bh;by++){int hc=(by*4+4<=h)?4:(h-by*4);for(int bx=0;bx<bw;bx++){int wc=(bx*4+4<=w)?4:(w-bx*4);
        const uint8_t* k=s+(by*bw+bx)*8;
        if(sg) bc4s(k,((int8_t*)d)+(by*4)*w+(bx*4),(size_t)w,1,wc,hc);
        else   bc4u(k,((uint8_t*)d)+(by*4)*w+(bx*4),(size_t)w,1,wc,hc);}}
}
static void bc5_img(const uint8_t* s,void* d,GLsizei w,GLsizei h,int sg){
    int bw=(w+3)/4,bh=(h+3)/4;
    for(int by=0;by<bh;by++){int hc=(by*4+4<=h)?4:(h-by*4);for(int bx=0;bx<bw;bx++){int wc=(bx*4+4<=w)?4:(w-bx*4);
        const uint8_t* k=s+(by*bw+bx)*16; size_t off=(size_t)(by*4)*(size_t)w*2+(size_t)(bx*4)*2;
        if(sg){bc4s(k,((int8_t*)d)+off,(size_t)w*2,2,wc,hc);bc4s(k+8,((int8_t*)d)+off+1,(size_t)w*2,2,wc,hc);}
        else  {bc4u(k,((uint8_t*)d)+off,(size_t)w*2,2,wc,hc);bc4u(k+8,((uint8_t*)d)+off+1,(size_t)w*2,2,wc,hc);}}}
}
typedef struct{int rgtc,bc5,sgn;GLenum ifmt,fmt,type;int ch;} rgtc_t;
static int rgtc_classify(GLenum f,rgtc_t* o){
    memset(o,0,sizeof(*o));
    switch(f){
    case GL_COMPRESSED_RED_RGTC1: case GL_COMPRESSED_LUMINANCE_LATC1_EXT:
        o->ifmt=GL_R8;o->fmt=GL_RED;o->type=GL_UNSIGNED_BYTE;o->ch=1;o->rgtc=1;return 1;
    case GL_COMPRESSED_SIGNED_RED_RGTC1: case GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT:
        o->ifmt=GL_R8_SNORM;o->fmt=GL_RED;o->type=GL_BYTE;o->ch=1;o->rgtc=1;o->sgn=1;return 1;
    case GL_COMPRESSED_RG_RGTC2: case GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT:
        o->ifmt=GL_RG8;o->fmt=GL_RG;o->type=GL_UNSIGNED_BYTE;o->ch=2;o->rgtc=1;o->bc5=1;return 1;
    case GL_COMPRESSED_SIGNED_RG_RGTC2: case GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT:
        o->ifmt=GL_RG8_SNORM;o->fmt=GL_RG;o->type=GL_BYTE;o->ch=2;o->rgtc=1;o->bc5=1;o->sgn=1;return 1;
    default: return 0; }
}

__attribute__((visibility("default")))
void glCompressedTexImage2D(GLenum t,GLint l,GLenum ifmt,GLsizei w,GLsizei h,GLint b,GLsizei sz,const void* d){
    rgtc_t in; if(!rgtc_classify(ifmt,&in)){
        static void(*r)(GLenum,GLint,GLenum,GLsizei,GLsizei,GLint,GLsizei,const void*)=0;
        if(!r)r=dlsym(g_gl,"glCompressedTexImage2D"); if(r)r(t,l,ifmt,w,h,b,sz,d); return; }
    size_t n=(size_t)w*h*in.ch; void* buf=malloc(n); if(!buf)return;
    if(in.bc5)bc5_img(d,buf,w,h,in.sgn); else bc4_img(d,buf,w,h,in.sgn);
    if(r_TexImage2D)r_TexImage2D(t,l,(GLint)in.ifmt,w,h,b,in.fmt,in.type,buf); free(buf);
}
__attribute__((visibility("default")))
void glCompressedTexImage3D(GLenum t,GLint l,GLenum ifmt,GLsizei w,GLsizei h,GLsizei dp,GLint b,GLsizei sz,const void* d){
    rgtc_t in; if(!rgtc_classify(ifmt,&in)){
        static void(*r)(GLenum,GLint,GLenum,GLsizei,GLsizei,GLsizei,GLint,GLsizei,const void*)=0;
        if(!r)r=dlsym(g_gl,"glCompressedTexImage3D"); if(r)r(t,l,ifmt,w,h,dp,b,sz,d); return; }
    size_t sl=(size_t)w*h*in.ch, bs=in.bc5?16:8; int bw=(w+3)/4,bh=(h+3)/4; size_t ss=(size_t)bw*bh*bs;
    uint8_t* buf=malloc(sl*dp); if(!buf)return;
    for(int z=0;z<dp;z++){ if(in.bc5)bc5_img((const uint8_t*)d+z*ss,buf+z*sl,w,h,in.sgn); else bc4_img((const uint8_t*)d+z*ss,buf+z*sl,w,h,in.sgn); }
    if(r_TexImage3D)r_TexImage3D(t,l,(GLint)in.ifmt,w,h,dp,b,in.fmt,in.type,buf); free(buf);
}
__attribute__((visibility("default")))
void glCompressedTexSubImage2D(GLenum t,GLint l,GLint xo,GLint yo,GLsizei w,GLsizei h,GLenum f,GLsizei sz,const void* d){
    rgtc_t in; if(!rgtc_classify(f,&in)){
        static void(*r)(GLenum,GLint,GLint,GLint,GLsizei,GLsizei,GLenum,GLsizei,const void*)=0;
        if(!r)r=dlsym(g_gl,"glCompressedTexSubImage2D"); if(r)r(t,l,xo,yo,w,h,f,sz,d); return; }
    size_t n=(size_t)w*h*in.ch; void* buf=malloc(n); if(!buf)return;
    if(in.bc5)bc5_img(d,buf,w,h,in.sgn); else bc4_img(d,buf,w,h,in.sgn);
    if(r_TexSubImage2D)r_TexSubImage2D(t,l,xo,yo,w,h,in.fmt,in.type,buf); free(buf);
}
__attribute__((visibility("default")))
void glCompressedTexSubImage3D(GLenum t,GLint l,GLint xo,GLint yo,GLint zo,GLsizei w,GLsizei h,GLsizei dp,GLenum f,GLsizei sz,const void* d){
    rgtc_t in; if(!rgtc_classify(f,&in)){
        static void(*r)(GLenum,GLint,GLint,GLint,GLint,GLsizei,GLsizei,GLsizei,GLenum,GLsizei,const void*)=0;
        if(!r)r=dlsym(g_gl,"glCompressedTexSubImage3D"); if(r)r(t,l,xo,yo,zo,w,h,dp,f,sz,d); return; }
    size_t sl=(size_t)w*h*in.ch, bs=in.bc5?16:8; int bw=(w+3)/4,bh=(h+3)/4; size_t ss=(size_t)bw*bh*bs;
    uint8_t* buf=malloc(sl*dp); if(!buf)return;
    for(int z=0;z<dp;z++){ if(in.bc5)bc5_img((const uint8_t*)d+z*ss,buf+z*sl,w,h,in.sgn); else bc4_img((const uint8_t*)d+z*ss,buf+z*sl,w,h,in.sgn); }
    if(r_TexSubImage3D)r_TexSubImage3D(t,l,xo,yo,zo,w,h,dp,in.fmt,in.type,buf); free(buf);
}

/* ---------- 3D sRGB textures: macOS doesn't support sRGB on GL_TEXTURE_3D -- */
#define GL_TEXTURE_3D 0x806F
#define GL_SRGB8_ALPHA8 0x8C43
#define GL_RGBA8 0x8058
static GLenum fix_3d_srgb(GLenum target, GLenum ifmt){
    if(target==GL_TEXTURE_3D && ifmt==GL_SRGB8_ALPHA8) return GL_RGBA8;
    return ifmt;
}
__attribute__((visibility("default")))
void glTexImage3D(GLenum t,GLint l,GLint ifmt,GLsizei w,GLsizei h,GLsizei dp,GLint b,GLenum f,GLenum ty,const void* d){
    GLenum nf=fix_3d_srgb(t,(GLenum)ifmt);
    if(r_TexImage3D)r_TexImage3D(t,l,(GLint)nf,w,h,dp,b,f,ty,d);
}

/* ---------- glTexStorage2D/3D: replace RGTC/LATC sized formats ---------- */
__attribute__((visibility("default")))
void glTexStorage2D(GLenum t,GLsizei lv,GLenum ifmt,GLsizei w,GLsizei h){
    rgtc_t in;
    static void(*r)(GLenum,GLsizei,GLenum,GLsizei,GLsizei)=0;
    if(!r)r=dlsym(g_gl,"glTexStorage2D");
    if(rgtc_classify(ifmt,&in)){ if(r)r(t,lv,in.ifmt,w,h); }
    else{ if(r)r(t,lv,ifmt,w,h); }
}
__attribute__((visibility("default")))
void glTexStorage3D(GLenum t,GLsizei lv,GLenum ifmt,GLsizei w,GLsizei h,GLsizei d){
    rgtc_t in;
    static void(*r)(GLenum,GLsizei,GLenum,GLsizei,GLsizei,GLsizei)=0;
    if(!r)r=dlsym(g_gl,"glTexStorage3D");
    GLenum f2=fix_3d_srgb(t,ifmt);
    if(rgtc_classify(f2,&in)){ if(r)r(t,lv,in.ifmt,w,h,d); }
    else{ if(r)r(t,lv,f2,w,h,d); }
}

/* ---------- legacy ALPHA/LUMINANCE textures -> R8/RG8 + swizzle ---------- */
typedef struct{int legacy;GLint ni;GLenum nf;GLint sr,sg,sb,sa;} leg_t;
static int leg_classify(GLint ifmt,GLenum fmt,leg_t* o){
    memset(o,0,sizeof(*o)); o->sr=GL_RED;o->sg=GL_GREEN;o->sb=0x1905;o->sa=0x1906;
    int A=(ifmt==0x1906||ifmt==0x803C||fmt==0x1906);
    int Lm=(ifmt==0x1909||ifmt==0x8040);
    int LA=(ifmt==0x190A||ifmt==0x8045||fmt==0x190A);
    int I=(ifmt==0x8049||ifmt==0x804B);
    if(LA){o->legacy=1;o->ni=GL_RG8;o->nf=GL_RG;o->sr=GL_RED;o->sg=GL_RED;o->sb=GL_RED;o->sa=GL_GREEN;}
    else if(A){o->legacy=1;o->ni=GL_R8;o->nf=GL_RED;o->sr=0;o->sg=0;o->sb=0;o->sa=GL_RED;}
    else if(Lm){o->legacy=1;o->ni=GL_R8;o->nf=GL_RED;o->sr=GL_RED;o->sg=GL_RED;o->sb=GL_RED;o->sa=1;}
    else if(I){o->legacy=1;o->ni=GL_R8;o->nf=GL_RED;o->sr=GL_RED;o->sg=GL_RED;o->sb=GL_RED;o->sa=GL_RED;}
    return o->legacy;
}
__attribute__((visibility("default")))
void glTexImage2D(GLenum t,GLint l,GLint ifmt,GLsizei w,GLsizei h,GLint b,GLenum f,GLenum ty,const void* d){
    leg_t lt;
    if(leg_classify(ifmt,f,&lt)){
        if(r_TexImage2D)r_TexImage2D(t,l,lt.ni,w,h,b,lt.nf,ty,d);
        if(r_TexParameteri){ r_TexParameteri(t,GL_TEXTURE_SWIZZLE_R,lt.sr); r_TexParameteri(t,GL_TEXTURE_SWIZZLE_G,lt.sg);
                             r_TexParameteri(t,GL_TEXTURE_SWIZZLE_B,lt.sb); r_TexParameteri(t,GL_TEXTURE_SWIZZLE_A,lt.sa); }
        return;
    }
    if(r_TexImage2D)r_TexImage2D(t,l,ifmt,w,h,b,f,ty,d);
}
__attribute__((visibility("default")))
void glTexSubImage2D(GLenum t,GLint l,GLint xo,GLint yo,GLsizei w,GLsizei h,GLenum f,GLenum ty,const void* d){
    GLenum nf=f; if(f==0x1906||f==0x1909)nf=GL_RED; else if(f==0x190A)nf=GL_RG;
    if(r_TexSubImage2D)r_TexSubImage2D(t,l,xo,yo,w,h,nf,ty,d);
}

/* ---------- GLSL: drop stray ';' after function definitions ---------- */
static char* fixup_glsl(const char* src){
    size_t n=strlen(src); char* out=malloc(n+1); if(!out)return 0; size_t oi=0; int depth=0; static int isfn[128];
    for(size_t i=0;i<n;i++){ char c=src[i];
        if(c=='{'){ int j=(int)oi-1; while(j>=0&&(out[j]==' '||out[j]=='\t'||out[j]=='\n'||out[j]=='\r'))j--;
            if(depth>=0&&depth<128) isfn[depth]=(j>=0&&out[j]==')'); depth++; out[oi++]=c; }
        else if(c=='}'){ depth--; out[oi++]=c; int wf=(depth>=0&&depth<128)?isfn[depth]:0;
            if(depth==0&&wf){ size_t k=i+1; while(k<n&&(src[k]==' '||src[k]=='\t'||src[k]=='\n'||src[k]=='\r'))k++;
                if(k<n&&src[k]==';'){ for(size_t wsp=i+1;wsp<k;wsp++)out[oi++]=src[wsp]; i=k; } } }
        else out[oi++]=c; }
    out[oi]=0; return out;
}
__attribute__((visibility("default")))
void glShaderSource(GLuint sh,GLsizei cnt,const char* const* str,const GLint* len){
    size_t tot=0; for(GLsizei i=0;i<cnt;i++) tot+=str[i]?((len&&len[i]>=0)?(size_t)len[i]:strlen(str[i])):0;
    char* j=malloc(tot+1);
    if(j){ size_t off=0; for(GLsizei i=0;i<cnt;i++) if(str[i]){ size_t l=(len&&len[i]>=0)?(size_t)len[i]:strlen(str[i]); memcpy(j+off,str[i],l); off+=l; } j[off]=0;
        char* fx=fixup_glsl(j);
        if(fx){ const char* one[1]={fx}; if(r_ShaderSource)r_ShaderSource(sh,1,one,0); free(fx); free(j); return; }
        free(j); }
    if(r_ShaderSource)r_ShaderSource(sh,cnt,str,len);
}
/* optional compile/link error reporting (gated) */
__attribute__((visibility("default")))
void glCompileShader(GLuint s){
    static void(*r)(GLuint)=0; if(!r)r=dlsym(g_gl,"glCompileShader"); if(r)r(s);
    if(g_log>0&&r_GetShaderiv){ GLint ok=1; r_GetShaderiv(s,GL_COMPILE_STATUS,&ok);
        if(!ok){ char b[2048]=""; if(r_GetShaderInfoLog)r_GetShaderInfoLog(s,sizeof(b),0,b); fprintf(stderr,"[rgtc_gl] shader %u compile FAILED:\n%s\n",s,b); } }
}
__attribute__((visibility("default")))
void glLinkProgram(GLuint p){
    static void(*r)(GLuint)=0; if(!r)r=dlsym(g_gl,"glLinkProgram"); if(r)r(p);
    if(g_log>0&&r_GetProgramiv){ GLint ok=1; r_GetProgramiv(p,GL_LINK_STATUS,&ok);
        if(!ok){ char b[2048]=""; if(r_GetProgramInfoLog)r_GetProgramInfoLog(p,sizeof(b),0,b); fprintf(stderr,"[rgtc_gl] program %u link FAILED:\n%s\n",p,b); } }
}

/* ---------- VAO emulation + client-array -> VBO translation ---------- */
#define MAXATTR 16
typedef struct{int en,client;GLint size;GLenum type;unsigned char norm;GLsizei stride;const void* ptr;} attr_t;
static attr_t g_attr[MAXATTR];
static GLuint g_arr_buf=0,g_elem_buf=0,g_cur_vao=0,g_default_vao=0,g_svbo[MAXATTR],g_sibo=0;
static int gl_tsize(GLenum t){ switch(t){case 0x1400:case 0x1401:return 1;case 0x1402:case 0x1403:case 0x140B:return 2;
    case 0x1404:case 0x1405:case 0x1406:return 4;case 0x140A:return 8;default:return 4;} }
static void ensure_vao(void){
    if(g_cur_vao!=0)return;
    if(g_default_vao==0&&r_GenVertexArrays)r_GenVertexArrays(1,&g_default_vao);
    if(g_default_vao&&r_BindVertexArray){ r_BindVertexArray(g_default_vao); g_cur_vao=g_default_vao; }
}
static int any_client(void){ for(int i=0;i<MAXATTR;i++) if(g_attr[i].en&&g_attr[i].client)return 1; return 0; }
static void setup_client(unsigned nv){
    for(int i=0;i<MAXATTR;i++){ if(!g_attr[i].en||!g_attr[i].client)continue;
        int esz=g_attr[i].size*gl_tsize(g_attr[i].type); int st=g_attr[i].stride?g_attr[i].stride:esz;
        size_t span=(size_t)nv*(size_t)st;
        if(g_svbo[i]==0&&r_GenBuffers)r_GenBuffers(1,&g_svbo[i]);
        r_BindBuffer(GL_ARRAY_BUFFER,g_svbo[i]); r_BufferData(GL_ARRAY_BUFFER,(long)span,g_attr[i].ptr,GL_STREAM_DRAW);
        r_VertexAttribPointer(i,g_attr[i].size,g_attr[i].type,g_attr[i].norm,g_attr[i].stride,(const void*)0); }
}
static unsigned scan_max(GLsizei c,GLenum t,const void* x){ unsigned m=0;
    if(t==0x1401){const uint8_t* p=x;for(GLsizei i=0;i<c;i++)if(p[i]>m)m=p[i];}
    else if(t==0x1403){const uint16_t* p=x;for(GLsizei i=0;i<c;i++)if(p[i]>m)m=p[i];}
    else{const uint32_t* p=x;for(GLsizei i=0;i<c;i++)if(p[i]>m)m=p[i];} return m; }
static const void* upload_idx(GLsizei c,GLenum t,const void* x){
    if(g_elem_buf!=0)return x;
    if(g_sibo==0&&r_GenBuffers)r_GenBuffers(1,&g_sibo);
    r_BindBuffer(GL_ELEMENT_ARRAY_BUFFER,g_sibo); r_BufferData(GL_ELEMENT_ARRAY_BUFFER,(long)c*gl_tsize(t),x,GL_STREAM_DRAW);
    return (const void*)0;
}
static void restore_buf(void){ r_BindBuffer(GL_ARRAY_BUFFER,g_arr_buf); r_BindBuffer(GL_ELEMENT_ARRAY_BUFFER,g_elem_buf); }

__attribute__((visibility("default")))
void glBindVertexArray(GLuint v){
    if(!r_BindVertexArray)return;
    if(v==0){ if(g_default_vao==0&&r_GenVertexArrays)r_GenVertexArrays(1,&g_default_vao); r_BindVertexArray(g_default_vao); g_cur_vao=g_default_vao; return; }
    r_BindVertexArray(v); g_cur_vao=v;
}
__attribute__((visibility("default")))
void glBindBuffer(GLenum tgt,GLuint b){ if(r_BindBuffer)r_BindBuffer(tgt,b);
    if(tgt==GL_ARRAY_BUFFER)g_arr_buf=b; else if(tgt==GL_ELEMENT_ARRAY_BUFFER)g_elem_buf=b; }
__attribute__((visibility("default")))
void glVertexAttribPointer(GLuint i,GLint s,GLenum t,unsigned char n,GLsizei st,const void* p){
    ensure_vao(); if(i<MAXATTR){g_attr[i].size=s;g_attr[i].type=t;g_attr[i].norm=n;g_attr[i].stride=st;g_attr[i].ptr=p;g_attr[i].client=(g_arr_buf==0);}
    if(g_arr_buf!=0&&r_VertexAttribPointer)r_VertexAttribPointer(i,s,t,n,st,p);
}
__attribute__((visibility("default")))
void glVertexAttribIPointer(GLuint i,GLint s,GLenum t,GLsizei st,const void* p){
    ensure_vao(); if(i<MAXATTR){g_attr[i].size=s;g_attr[i].type=t;g_attr[i].norm=0;g_attr[i].stride=st;g_attr[i].ptr=p;g_attr[i].client=(g_arr_buf==0);}
    if(g_arr_buf!=0&&r_VertexAttribIPointer)r_VertexAttribIPointer(i,s,t,st,p);
}
__attribute__((visibility("default")))
void glEnableVertexAttribArray(GLuint i){ ensure_vao(); if(i<MAXATTR)g_attr[i].en=1; if(r_EnableVertexAttribArray)r_EnableVertexAttribArray(i); }
__attribute__((visibility("default")))
void glDisableVertexAttribArray(GLuint i){ ensure_vao(); if(i<MAXATTR)g_attr[i].en=0; if(r_DisableVertexAttribArray)r_DisableVertexAttribArray(i); }

__attribute__((visibility("default")))
void glDrawArrays(GLenum m,GLint f,GLsizei c){ ensure_vao();
    if(any_client()){ setup_client((unsigned)(f+c)); if(r_DrawArrays)r_DrawArrays(m,f,c); restore_buf(); }
    else if(r_DrawArrays)r_DrawArrays(m,f,c); }
__attribute__((visibility("default")))
void glDrawElements(GLenum m,GLsizei c,GLenum t,const void* ix){ ensure_vao();
    if((g_elem_buf==0)||any_client()){ if(any_client())setup_client(scan_max(c,t,ix)+1); const void* ip=upload_idx(c,t,ix);
        if(r_DrawElements)r_DrawElements(m,c,t,ip); restore_buf(); }
    else if(r_DrawElements)r_DrawElements(m,c,t,ix); }
__attribute__((visibility("default")))
void glDrawRangeElements(GLenum m,GLuint s,GLuint e,GLsizei c,GLenum t,const void* ix){ ensure_vao();
    if((g_elem_buf==0)||any_client()){ if(any_client())setup_client(e+1); const void* ip=upload_idx(c,t,ix);
        if(r_DrawRangeElements)r_DrawRangeElements(m,s,e,c,t,ip); restore_buf(); }
    else if(r_DrawRangeElements)r_DrawRangeElements(m,s,e,c,t,ix); }
__attribute__((visibility("default")))
void glDrawElementsInstanced(GLenum m,GLsizei c,GLenum t,const void* ix,GLsizei n){ ensure_vao();
    if((g_elem_buf==0)||any_client()){ if(any_client())setup_client(scan_max(c,t,ix)+1); const void* ip=upload_idx(c,t,ix);
        if(r_DrawElementsInstanced)r_DrawElementsInstanced(m,c,t,ip,n); restore_buf(); }
    else if(r_DrawElementsInstanced)r_DrawElementsInstanced(m,c,t,ix,n); }
__attribute__((visibility("default")))
void glDrawArraysInstanced(GLenum m,GLint f,GLsizei c,GLsizei n){ ensure_vao();
    if(any_client()){ setup_client(f+c); if(r_DrawArraysInstanced)r_DrawArraysInstanced(m,f,c,n); restore_buf(); }
    else if(r_DrawArraysInstanced)r_DrawArraysInstanced(m,f,c,n); }

/* ---------- advertise RGTC capability ---------- */
__attribute__((visibility("default")))
void glGetIntegerv(GLenum pn,GLint* p){
    if(pn==GL_NUM_COMPRESSED_TEXTURE_FORMATS){ GLint r=0; if(r_GetIntegerv)r_GetIntegerv(pn,&r); if(r<0)r=0; if(p)*p=r+kRgtcFormatCount; return; }
    if(pn==GL_COMPRESSED_TEXTURE_FORMATS){ GLint r=0; if(r_GetIntegerv){r_GetIntegerv(GL_NUM_COMPRESSED_TEXTURE_FORMATS,&r);r_GetIntegerv(pn,p);} if(r<0)r=0;
        if(p)for(int i=0;i<kRgtcFormatCount;i++)p[r+i]=kRgtcFormats[i]; return; }
    if(pn==GL_NUM_EXTENSIONS){ GLint r=0; if(r_GetIntegerv)r_GetIntegerv(pn,&r); if(r<0)r=0; if(p)*p=r+kInjectedExtCount; return; }
    if(r_GetIntegerv)r_GetIntegerv(pn,p);
}
static char* g_extstr=0;
__attribute__((visibility("default")))
const GLubyte* glGetString(GLenum n){
    const GLubyte* base=r_GetString?r_GetString(n):0; if(n!=GL_EXTENSIONS)return base;
    if(g_extstr)return (const GLubyte*)g_extstr;
    const char* b=(const char*)base; size_t bl=b?strlen(b):0,ex=0; for(int i=0;i<kInjectedExtCount;i++)ex+=strlen(kInjectedExts[i])+1;
    char* s=malloc(bl+1+ex+1); if(!s)return base; s[0]=0; if(b&&bl){strcpy(s,b);strcat(s," ");}
    for(int i=0;i<kInjectedExtCount;i++){strcat(s,kInjectedExts[i]);if(i+1<kInjectedExtCount)strcat(s," ");}
    g_extstr=s; return (const GLubyte*)s;
}
__attribute__((visibility("default")))
const GLubyte* glGetStringi(GLenum n,GLuint i){
    if(n!=GL_EXTENSIONS)return r_GetStringi?r_GetStringi(n,i):0;
    GLint num=0; if(r_GetIntegerv)r_GetIntegerv(GL_NUM_EXTENSIONS,&num);
    if((GLint)i<num)return r_GetStringi?r_GetStringi(n,i):0;
    GLuint loc=i-(GLuint)num; if(loc>=kInjectedExtCount)return 0; return (const GLubyte*)kInjectedExts[loc];
}
RGTC_GL_SOURCE_EOF

say "Compiling $DYLIB (universal x86_64 + arm64)"
clang -O2 -Wall -Wno-deprecated-declarations -arch x86_64 -arch arm64 -dynamiclib \
      -install_name "$DYLIB" -reexport_framework OpenGL \
      -o "$TMP/rgtc_gl.dylib" "$TMP/rgtc_gl.c"
mkdir -p "$(dirname "$DYLIB")"
cp "$TMP/rgtc_gl.dylib" "$DYLIB"
say "Installed $DYLIB"

# --- 2. patch winemac.so (idempotent, pattern-based) ------------------------
# Refresh a pristine backup whenever winemac.so is currently unpatched.
if grep -q '/System/Library/Frameworks/OpenGL.framework/OpenGL' "$WINEMAC" 2>/dev/null; then
    cp "$WINEMAC" "$WINEMAC.rgtc-backup"
    say "Backed up pristine winemac.so -> winemac.so.rgtc-backup"
fi

say "Patching winemac.so"
python3 - "$WINEMAC" "$DYLIB" <<'PYEOF'
import sys
path, dylib = sys.argv[1], sys.argv[2]
d = bytearray(open(path, 'rb').read())
changed = False

def find(pat):
    return d.find(bytes.fromhex(pat))

# (a) force core GL profile (older stable CrossOver): mov r12d,1 -> mov r12d,3
#     anchor: 41 BC 01 00 00 00 45 31 FF  (mov r12d,1 ; xor r15d,r15d)
#     Newer builds carry the requested GL major in a different register with no
#     constant immediate here, so this lever is best-effort, not mandatory.
core_ok = False
i = find('41bc010000004531ff')
if i >= 0:
    d[i+2] = 0x03; changed = True; core_ok = True; print("  [core-profile] r12d=1 -> 3")
elif find('41bc030000004531ff') >= 0:
    core_ok = True; print("  [core-profile] already set (r12d=3)")
else:
    print("  [core-profile] r12d immediate absent (newer build; using jb-nop lever)")

# (a2) The reliable lever (CrossOver Preview overwrites the profile-major local):
#      NOP the conditional jump that skips adding the core-profile pixel-format
#      attribute, forcing kCGLPFAOpenGLProfile into every game context.
#      The compare register shifted r12d -> ebx across builds; handle both.
#        r12d form: cmp r12d,3 ; jb +0x27 ; mov eax,ecx  (41 83 FC 03 72 27 89 C8)
#        ebx  form: cmp ebx,3  ; jb +0x27 ; mov eax,ecx  (83 FB 03 72 27 89 C8)
for tag, jb_pat, jb_fix, off in [
    ("r12d", '4183fc03722789c8', '4183fc03909089c8', 4),
    ("ebx",  '83fb03722789c8',   '83fb03909089c8',   3),
]:
    i = find(jb_pat)
    if i >= 0:
        d[i+off] = 0x90; d[i+off+1] = 0x90; changed = True; core_ok = True
        print("  [core-profile-2] (%s) jb -> nop nop (force core profile in pixel format)" % tag)
        break
    if find(jb_fix) >= 0:
        core_ok = True; print("  [core-profile-2] (%s) already forced" % tag); break
else:
    print("  [core-profile-2] jb pattern not found")

if not core_ok:
    sys.exit("  [core-profile] no core-profile lever matched - unsupported winemac.so build")

# (b) strip RTLD_NOLOAD from the OpenGL dlopen: mov esi,0x15 -> mov esi,0x05
#     anchor: BE 15 00 00 00 E8  (mov esi,0x15 ; call dlopen)
i = find('be15000000e8')
if i >= 0:
    d[i+1] = 0x05; changed = True; print("  [dlopen-flag] RTLD_NOLOAD stripped")
elif find('be05000000e8') >= 0:
    print("  [dlopen-flag] already stripped")
else:
    sys.exit("  [dlopen-flag] PATTERN NOT FOUND - unsupported winemac.so build")

# (c) redirect the OpenGL dlopen path to our facade
oldstr = b'/System/Library/Frameworks/OpenGL.framework/OpenGL'  # 50 bytes, no NUL
i = d.find(oldstr)
if i >= 0:
    nb = dylib.encode()
    if len(nb) > len(oldstr):
        sys.exit("  [dlopen-path] facade path too long")
    d[i:i+len(oldstr)] = nb + b'\x00' * (len(oldstr) - len(nb))
    changed = True; print("  [dlopen-path] OpenGL.framework -> %s" % dylib)
elif dylib.encode() in d:
    print("  [dlopen-path] already redirected")
else:
    sys.exit("  [dlopen-path] PATTERN NOT FOUND - unsupported winemac.so build")

if changed:
    open(path, 'wb').write(d)
    print("  winemac.so written")
else:
    print("  winemac.so already fully patched")
PYEOF

say "Re-signing winemac.so (ad-hoc)"
codesign --remove-signature "$WINEMAC" 2>/dev/null || true
codesign -s - --force "$WINEMAC"

# --- 2b. patch opengl32.dll (PE-side, pattern-based) ------------------------
# CrossOver Preview's PE-side wglGetProcAddress has a version check that blocks
# GL 3.0+ functions when the cached context version is 2.1.  Bypass it.
WINE_WIN="$CROSSOVER_APP/Contents/SharedSupport/CrossOver/lib/wine/x86_64-windows"
GL32="$WINE_WIN/opengl32.dll"
if [ -f "$GL32" ]; then
    # Back up pristine opengl32.dll (only when unpatched)
    if python3 -c "import sys;d=open(sys.argv[1],'rb').read();sys.exit(0 if d.find(bytes.fromhex('410fb74424106685c0755b'))>=0 else 1)" "$GL32" 2>/dev/null; then
        cp "$GL32" "$GL32.rgtc-backup"
        say "Backed up pristine opengl32.dll -> opengl32.dll.rgtc-backup"
    fi
    say "Checking opengl32.dll for version-check patch"
    python3 - "$GL32" <<'GL32PYEOF'
import sys
path = sys.argv[1]
d = bytearray(open(path, 'rb').read())

# wglGetProcAddress version check:
#   movzx eax, word ptr [r12+0x10]  (version_major from table)
#   test ax, ax
#   jne +0x5b  → jumps to version comparison
# Patch: change jne displacement from 0x5b to 0x28 → jumps to success
pat = bytes.fromhex('410fb74424106685c0755b')
i = d.find(pat)
if i >= 0:
    d[i+10] = 0x28
    open(path, 'wb').write(d)
    print("  [wglGetProcAddr] version check bypassed (jne 0x5b -> 0x28)")
elif d.find(bytes.fromhex('410fb74424106685c07528')) >= 0:
    print("  [wglGetProcAddr] already patched")
else:
    print("  [wglGetProcAddr] pattern not found — not CrossOver Preview, skipping")
GL32PYEOF
fi

cat <<DONE

$(printf '\033[1;32mDone.\033[0m') Exanima is patched.

Remaining manual step:
  * Set the bottle's RetinaMode to OFF, or the UI will be mis-scaled:
      CrossOver  ->  bottle  ->  Wine Configuration  (or the registry key
      HKCU\\Software\\Wine\\Mac Driver\\RetinaMode = "n")

Then launch Exanima normally through Steam / CrossOver. No wrapper or env
vars are needed. For diagnostics, launch with RGTC_GL_LOG=1 in the
environment to log shader/texture issues to stderr.

If a CrossOver update makes the game crash again, just re-run this script.
DONE
