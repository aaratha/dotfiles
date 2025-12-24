-- Pull in the wezterm API
local wezterm = require("wezterm")
local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.font = wezterm.font({ family = "JetBrainsMono Nerd Font", weight = "Regular" })

-- Tabline Configuration
tabline.setup({
	options = {
		icons_enabled = true,
		theme = "Catppuccin Mocha",
		tabs_enabled = true,
		theme_overrides = {},
		section_separators = {
			left = wezterm.nerdfonts.ple_right_half_circle_thick,
			right = wezterm.nerdfonts.ple_left_half_circle_thick,
		},
		component_separators = {
			left = wezterm.nerdfonts.ple_right_half_circle_thin,
			right = wezterm.nerdfonts.ple_left_half_circle_thin,
		},
		tab_separators = {
			left = wezterm.nerdfonts.ple_right_half_circle_thick,
			right = wezterm.nerdfonts.ple_left_half_circle_thick,
		},
	},
	-- sections = {
	-- 	tabline_a = { "mode" },
	-- 	tabline_b = {
	-- 		"workspace",
	-- 		{ Foreground = { AnsiColor = "Fuchsia" } },
	-- 		{ Background = { Color = "blue" } },
	-- 	},
	-- 	tabline_c = { " " },
	-- 	tab_active = {
	-- 		"index",
	-- 		{ "parent", padding = 0 },
	-- 		"/",
	-- 		{ "cwd", padding = { left = 0, right = 1 } },
	-- 		{ "zoomed", padding = 0 },
	-- 	},
	-- 	tab_inactive = { "index", { "process", padding = { left = 0, right = 1 } } },
	-- 	tabline_x = { "ram", "cpu" },
	-- 	tabline_y = { "datetime", "battery" },
	-- 	tabline_z = {
	-- 		"domain",
	-- 		{ Background = { Color = "#000000" } },
	-- 		{ Foreground = { AnsiColor = "#000000" } },
	-- 	},
	-- },
	sections = {
		tabline_a = {},
		tabline_b = {},
		tabline_c = {},
		tabline_y = {
			"datetime",
			style = "%H:%M",
			"battery",
		},
		tabline_z = {},
	},

	extensions = {},
})

tabline.apply_to_config(config)
config.window_decorations = "RESIZE | MACOS_FORCE_ENABLE_SHADOW"
config.tab_bar_at_bottom = true

config.initial_cols = 90
config.initial_rows = 36

config.font_size = 14

config.color_scheme = "catppuccin-mocha"
config.colors = {
	background = "#000a0f",
	cursor_bg = "#ffffff",
	tab_bar = {
		background = "#000a0f",
	},
}

config.window_background_opacity = 0.7
config.macos_window_background_blur = 40

config.cursor_thickness = 2.0

----------------------Key Bindings----------------------
-- timeout_milliseconds defaults to 1000 and can be omitted
config.leader = { key = "`" }
config.keys = {
	{ -- split horizontal
		key = "|",
		mods = "LEADER",
		action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
	},
	{ -- split vertical
		key = "_",
		mods = "LEADER",
		action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
	},
	{ -- next tab
		key = "p",
		mods = "LEADER",
		action = wezterm.action.ActivateTabRelative(-1),
	},
	{ -- previous tab
		key = "n",
		mods = "LEADER",
		action = wezterm.action.ActivateTabRelative(1),
	},
	{
		key = "q",
		mods = "LEADER",
		action = wezterm.action.CloseCurrentPane({ confirm = true }),
	},
	-- Window Pane Navigation
	{
		key = "h",
		mods = "LEADER",
		action = wezterm.action.ActivatePaneDirection("Left"),
	},
	{
		key = "l",
		mods = "LEADER",
		action = wezterm.action.ActivatePaneDirection("Right"),
	},
	{
		key = "j",
		mods = "LEADER",
		action = wezterm.action.ActivatePaneDirection("Down"),
	},
	{
		key = "k",
		mods = "LEADER",
		action = wezterm.action.ActivatePaneDirection("Up"),
	},
}

return config
