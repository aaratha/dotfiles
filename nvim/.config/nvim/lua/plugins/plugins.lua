return {
  {
    "craftzdog/solarized-osaka.nvim",
    branch = "osaka",
    lazy = true,
    priority = 1000,
    opts = function()
      return {
        transparent = true,
      }
    end,
  },

  { "ellisonleao/gruvbox.nvim", opts = {
    transparent_mode = true,
  } },

  {
    "catppuccin/nvim",
    name = "catppuccin",
    opts = {
      transparent_background = true,
      integrations = {
        bufferline = true, -- enables bufferline integration automatically
      },
      float = {
        transparent = false,
      },
      dim_inactive = {
        enabled = false, -- dims the background color of inactive window
      },
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },

  -- For `plugins.lua` users.
  {
    "OXY2DEV/markview.nvim",
    lazy = false,

    -- For `nvim-treesitter` users.
    priority = 49,

    -- For blink.cmp's completion
    -- source
    -- dependencies = {
    --     "saghen/blink.cmp"
    -- },
  },

  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        ["html"] = { "prettier" },
      },
    },
  },

  {
    "folke/snacks.nvim",
    opts = {
      picker = {
        sources = {
          explorer = {
            layout = {
              layout = {
                width = 24,
              },
            },
          },
        },
      },
      zen = {
        toggles = { dim = false },
        on_open = function(win)
          -- disable line numbers
          vim.wo.number = false
          vim.wo.relativenumber = false

          -- enable wrap + breakindent when Zen opens
          vim.wo.wrap = true
          vim.wo.linebreak = true
          vim.wo.breakindent = true
          vim.wo.breakindentopt = "shift:2" -- Shift wrapped lines by 2 spaces

          -- Toggle off tmux status line
          if vim.env.TMUX then
            vim.fn.system("tmux set -g status off")
          end

          -- Hide fold arrows on the left side
          vim.wo.foldcolumn = "0"
          vim.wo.signcolumn = "no"

          -- Toggle indent guides off
          Snacks.toggle.indent():toggle()
        end,

        on_close = function()
          -- restore defaults after Zen closes
          vim.wo.wrap = false
          vim.wo.linebreak = false
          vim.wo.breakindent = false

          if vim.env.TMUX then
            vim.fn.system("tmux set -g status on")
          end

          vim.wo.foldcolumn = "1" -- or "auto"
          vim.wo.signcolumn = "yes"

          Snacks.toggle.indent():toggle()
        end,
      },
      styles = {
        zen = {
          enter = true,
          fixbuf = false,
          minimal = false,
          width = 80,
          height = 0,
          backdrop = { transparent = true, blend = 40 },
          keys = { q = false },
          zindex = 40,
          wo = {
            winhighlight = "NormalFloat:Normal",
          },
          w = {
            snacks_main = true,
          },
        },
      },
    },
  },

  {
    "ariedov/android-nvim",
    config = function()
      -- OPTIONAL: specify android sdk directory
      vim.g.android_sdk = "~/Library/Android/sdk"
      require("android-nvim").setup()
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "org",
        "bash",
        "c",
        "cpp",
        "diff",
        "html",
        "javascript",
        "jsdoc",
        "json",
        "jsonc",
        "lua",
        "luadoc",
        "luap",
        "markdown",
        "markdown_inline",
        "printf",
        "python",
        "query",
        "regex",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "xml",
        "yaml",
      },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = { "org" },
      },
    },
  },

  {
    "nvim-orgmode/orgmode",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    --event = "VeryLazy",
    event = "BufRead", -- replace VeryLazy, otherwise syntax highlighting won't work
    ft = { "org" },
    config = function()
      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/OneDrive/org/**/*",
        org_default_notes_file = "~/OneDrive/org/refile.org",
        org_hide_emphasis_markers = true,
        org_highlight_latex_and_related = "entities",
      })
    end,
  },

  {
    "00msjr/nvim-fountain",
    ft = "fountain", -- Lazy-load only for fountain files
    config = function()
      require("nvim-fountain").setup({
        -- Optional configuration
        keymaps = {
          next_scene = "]]",
          prev_scene = "[[",
          uppercase_line = "<S-CR>",
        },
        -- Export configuration
        export = {
          pdf = { options = "--overwrite" },
        },
      })
    end,
  },

  {
    "akinsho/org-bullets.nvim",
    config = function()
      require("org-bullets").setup()
    end,
  },

  {
    "smoka7/multicursors.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvimtools/hydra.nvim",
    },
    opts = {},
    cmd = { "MCstart", "MCvisual", "MCclear", "MCpattern", "MCvisualPattern", "MCunderCursor" },
    keys = {
      {
        mode = { "v", "n" },
        "<Leader>m",
        "<cmd>MCstart<cr>",
        desc = "Create a selection for selected text or word under the cursor",
      },
    },
  },

  {
    "aaratha/org-cycle-lite.nvim",
    lazy = false,
    dependencies = { "nvim-orgmode/orgmode" },
    config = function()
      require("org-cycle-lite").setup()
    end,
  },
}
