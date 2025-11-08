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
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      -- Setup orgmode
      require("orgmode").setup({
        org_agenda_files = "~/orgfiles/**/*",
        org_default_notes_file = "~/orgfiles/refile.org",
      })

      -- NOTE: If you are using nvim-treesitter with ~ensure_installed = "all"~ option
      -- add ~org~ to ignore_install
      -- require('nvim-treesitter.configs').setup({
      --   ensure_installed = 'all',
      --   ignore_install = { 'org' },
      -- })
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
}
