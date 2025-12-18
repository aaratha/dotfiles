-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Org files

vim.keymap.set("n", "<leader>fo", "", { desc = "Org" })

vim.keymap.set("n", "<leader>fon", function()
  vim.cmd("edit ~/OneDrive/org/notes.org")
end, { desc = "Personal Notes" })

vim.keymap.set("n", "<leader>fow", function()
  vim.cmd("edit ~/OneDrive/org/notes.org")
end, { desc = "Work Notes" })

-- custom zen-mode
--
-- local zen_state = {}
--
-- vim.keymap.set("n", "<leader>uz", function()
--   local win = vim.api.nvim_get_current_win()
--
--   if zen_state[win] then
--     -- Restore previous state
--     local s = zen_state[win]
--     if vim.api.nvim_win_is_valid(win) then
--       for k, v in pairs(s) do
--         vim.wo[win][k] = v
--       end
--     end
--     zen_state[win] = nil
--   else
--     -- Save current state
--     zen_state[win] = {
--       wrap = vim.wo[win].wrap,
--       number = vim.wo[win].number,
--       relativenumber = vim.wo[win].relativenumber,
--       signcolumn = vim.wo[win].signcolumn,
--       colorcolumn = vim.wo[win].colorcolumn,
--     }
--
--     -- Apply zen-like settings
--     vim.wo[win].wrap = true
--     vim.wo[win].number = false
--     vim.wo[win].relativenumber = false
--     vim.wo[win].signcolumn = "no"
--     vim.wo[win].colorcolumn = "80"
--   end
-- end, { desc = "Toggle Zen (split-local)" })
