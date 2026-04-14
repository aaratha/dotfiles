if status is-interactive
    # Commands to run in interactive sessions can go here
end

function ..
    cd ..
end
function ...
    cd ../..
end
function ....
    cd ../../..
end
function .....
    cd ../../../..
end

set -x JAVA_HOME (/usr/libexec/java_home -v 25)
set -x PATH $JAVA_HOME/bin $PATH

# For lsp-booster to work in emacs
launchctl setenv LSP_USE_PLISTS true

source "$HOME/.cargo/env.fish"

zoxide init fish | source
starship init fish | source


# set -gx ANTHROPIC_AUTH_TOKEN "test"
# set -gx ANTHROPIC_BASE_URL "http://127.0.0.1:3456"
set -gx NO_PROXY "127.0.0.1"
set -gx DISABLE_TELEMETRY "true"
set -gx DISABLE_COST_WARNINGS "true"
set -gx API_TIMEOUT_MS "600000"
set -e CLAUDE_CODE_USE_BEDROCK



# pnpm
set -gx PNPM_HOME "/Users/aaratha/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
