vim.opt.backup=true
vim.opt.pastetoggle="<insert>"

vim.opt.relativenumber=true
vim.opt.number=true

vim.opt.shiftwidth=4
vim.opt.softtabstop=4
vim.opt.tabstop=4
vim.opt.expandtab = true

vim.opt.tabpagemax=20
vim.opt.tags="./tags,tags;"
vim.opt.showcmd=true
vim.opt.hidden=true
vim.opt.termguicolors=true
vim.opt.laststatus=2

vim.opt.foldenable=false
vim.opt.foldtext=PlainTextFold
vim.opt.foldmethod="syntax"

vim.opt.signcolumn="yes"
vim.opt.updatetime=300

vim.opt.jumpoptions:append {"stack"}

vim.g.netrw_banner=0
vim.g.netrw_liststyle=3
-- disable netrw (using nvim-tree/neo-tree instead)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

function Filter(values, filter_fn)
    local result = {}
    for key, value in ipairs(values) do
        if filter_fn(value) then
            table.insert(result, value)
        end
    end
    return result
end

function Comma(...)
    local filtered_items = {}
    for key, value in ipairs({...}) do
        local item = tostring(value)
        if item ~= '' then
            table.insert(filtered_items, item)
        end
    end
    return table.concat(filtered_items, ',')
end

function Space(line)
    if line ~= '' then
        return line .. ' '
    end
    return ''
end

function Bracket(line)
    if line ~= '' then
        return '[' .. line .. ']'
    end
    return ''
end

function True(attr,text)
    if attr then
        return text
    end
    return ''
end

function Diff(one,other)
    if one ~= other then
        return one
    end
    return ''
end

function Equal(one,other)
    if one == other then
        return one
    end
    return ''
end

function StatuslineCurrentHighlight()
    local line = vim.fn.line('.')
    local col = vim.fn.col('.')
    local synID = vim.fn.synID(line,col,1)
    local name = vim.fn.synIDattr(synID,'name')

    if name ~= '' then
        return Bracket(name)
    end
    return ''
end

function VCSChanges()
    local hunks = vim.fn['sy#repo#get_stats']()
    local parts = {}

    if hunks[0] ~= nil and hunks[0] > 0 then
        local add = vim.g.signify_sign_add or '+'
        table.insert(parts, add .. hunks[0])
    end

    if hunks[1] ~= nil and hunks[1] > 0 then
        local change = vim.g.signify_sign_change or '!'
        table.insert(parts, change .. hunks[1])
    end

    if hunks[2] ~= nil and hunks[2] > 0 then
        local delete = vim.g.signify_sign_delete_first_line or '-'
        table.insert(parts, delete .. hunks[2])
    end

    return table.concat(parts)
end

vim.api.nvim_create_autocmd(
    {'cursorhold', 'bufwritepost'},
    {
        -- reset the cache on writes
        callback = function() vim.b.statusline_trailing_space_warning = nil end,
    }
)
function TrailingSpace()
    -- scans the file and checks if any line ends has trailling spaces
    if vim.b.statusline_trailing_space_warning == nil then
        if not vim.bo.modifiable then
            vim.b.statusline_trailing_space_warning = ''
            return vim.b.statusline_trailing_space_warning
        end

        -- for very large files the search can take a long time, hanging the UI
        -- thread.
        local timeout_ms=50
        -- 0 means search in the whole file
        local stop_line=0
        if vim.fn.search('[[:space:]]$', 'nw', stop_line, timeout_ms) ~= 0 then
            vim.b.statusline_trailing_space_warning = '\\s$'
        else
            vim.b.statusline_trailing_space_warning = ''
        end
    end
    return vim.b.statusline_trailing_space_warning
end

vim.api.nvim_create_autocmd(
    {'cursorhold', 'bufwritepost'},
    {
        callback = function() vim.b.statusline_tab_warning = nil end
    }
)
function MixedTabSpace()
    -- scans the file and checks if both tabs and spaces are being used for identation
    if vim.b.statusline_tab_warning == nil then
        vim.b.statusline_tab_warning = ''

        if not vim.bo.modifiable then
            return vim.b.statusline_tab_warning
        end

        -- if any line starts with a tab
        local tabs = vim.fn.search('^[[:tab:]]', 'nw') ~= 0
        -- if any line starts with enough spaces for a tab
        -- note: should probably also check for fewer than tabstop spaces followed by a tab
        local spaces = vim.fn.search('^ \\{' .. vim.o.tabstop .. '}', 'nw') ~= 0

        if tabs and spaces then
            vim.b.statusline_tab_warning = 'mixed-indenting'
        elseif (spaces and not vim.o.expandtab) or (tabs and vim.o.expandtab) then
            vim.b.statusline_tab_warning = '&et'
        end
    end
    return vim.b.statusline_tab_warning
end

function LoclistErrors()
    local loclist = vim.fn.getloclist(0)
    local errors = Filter(loclist, function(v) return v.type == 'E' end)
    local num_errors = #errors
    if num_errors > 0 then
        return num_errors .. 'E@' .. errors[1]['lnum']
    end
end

function LoclistWarnings()
    local loclist = vim.fn.getloclist(0)
    local warnings = Filter(loclist, function(v) return v.type == 'W' end)
    local num_warnings = #warnings
    if num_warnings > 0 then
        return num_warnings .. 'W@' .. warnings[1]['lnum']
    end
end

function StatusLineWarnings()
    local errors = LoclistErrors()
    local warnings = LoclistWarnings()
    local tab_space = MixedTabSpace()
    local trailling_space = TrailingSpace()
    local formatted = Space(Bracket(Comma(errors,warnings,tab_space,trailling_space)))
    return formatted
end

function StatusLineConfigs()
    local paste = True(vim.go.paste,'paste')
    local readonly = True(vim.go.readonly,'RO')
    local fileformat = Diff(vim.go.fileformat, 'unix')
    local fileencoding = Diff(vim.go.fileencoding, 'utf-8')
    local filetype = vim.go.filetype
    local formatted = Space(Bracket(Comma(paste,readonly,fileformat,fileencoding,filetype)))
    return formatted
end

function VirtualEnv()
    return Space(vim.fn['virtualenv#statusline']())
end

function StatusLineVCS()
    return Space(VCSChanges() .. vim.fn.FugitiveHead(7))
end

function PlainTextFold()
    local line = vim.fn.getline(vim.v.foldstart)
    -- local sub = vim.fn.substitute(line, '/\*\|\*/\|{{{\d\=', '', 'g')
    return vim.v.folddashes .. sub
end

function TreeSitterUpdateParsers()
    local info = require('nvim-treesitter.info')
    local installed_parsers = info.installed_parsers()
    local target_parsers = {'bash', 'python', 'rust', 'regex', 'json', 'lua', 'cpp', 'vim'}

    local configs = require('nvim-treesitter.configs')

    configs.setup {
        ensure_installed = target_parsers,
        sync_install = false,
        auto_install = true,
        ignore_install = {},
        highlight = {
            enable = true,
            disable = {},
            additional_vim_regex_highlighting = false,
        },
        incremental_selection = {
            enable = true,
            keymaps = {
                init_selection = "gnn",
                node_incremental = "grn",
                scope_incremental = "grc",
                node_decremental = "grm",
            },
        },
    }

    -- must run after setup because it calls :TSUninstall
    -- local parsers_to_remove = {}
    -- for _, parser in ipairs(installed_parsers) do
    --     if vim.fn.index(target_parsers, parser) == -1 then
    --         table.insert(parsers_to_remove, parser)
    --     end
    -- end
    -- if #parsers_to_remove > 0 then
    --     vim.cmd('TSUninstall ' .. table.concat(parsers_to_remove, ' '))
    -- end
end

function StatusLineSetup()
    -- this requires vim-signify for VCSChanges
    -- this requires vim-virtualenv for VirtualEnv
    -- this requires vim-fugitive for StatusLineVCS

    -- Only configure statusline after the plugins have been installed, otherwise
    -- lots of errors are shown

    local statusline
    statusline="%m"

    -- warns for readonly, syntax erros, files not ending in \n and files that are
    -- not utf8
    statusline=statusline .. "%#warningmsg#"
    statusline=statusline .. "%{v:lua.StatusLineWarnings()}"
    statusline=statusline .. "%*"

    statusline=statusline .. " %t "

    statusline=statusline .. "%{v:lua.StatusLineConfigs()}"
    statusline=statusline .. "%{v:lua.VirtualEnv()}"
    statusline=statusline .. "%{v:lua.StatusLineVCS()}"

    statusline=statusline .. "%="
    statusline=statusline .. "%<"

    statusline=statusline .. "%{v:lua.StatuslineCurrentHighlight()}"
    statusline=statusline .. " [ascii %03.3b hex %02.2B]"
    statusline=statusline .. " [col %v line %l/%L %p%%]"
    vim.opt.statusline = statusline
end

function CmpSetup()
    local cmp = require('cmp')
    local luasnip = require('luasnip')
    local cmp_nvim_lsp = require('cmp_nvim_lsp')
    local lspconfig = require('lspconfig')

    cmp.setup({
        snippet = {
            expand = function(args)
                luasnip.lsp_expand(args.body)
            end,
        },
        mapping = cmp.mapping.preset.insert({
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'luasnip' },
        }, {
            { name = 'buffer' },
        })
    })

    -- requires hrsh7th/cmp-cmdline
    cmp.setup.cmdline('/', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
            { name = 'buffer' }
        }
    })

    -- requires hrsh7th/cmp-cmdline
    cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
            { name = 'path' }
        }, {
            { name = 'cmdline' }
        })
    })

    local capabilities = cmp_nvim_lsp.default_capabilities()

    local on_attach = function(client, bufnr)
        local bufopts = { noremap=true, silent=true, buffer=bufnr }
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)

        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)

        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
        vim.keymap.set('n', '<Space>a', vim.lsp.buf.code_action, bufopts)

        vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

        vim.lsp.inlay_hint(bufnr, true)
    end

    lspconfig['rust_analyzer'].setup{
        capabilities = capabilities,
        on_attach = on_attach,
        settings = {
            ['rust-analyzer'] = {
                cargo = {
                    features = "all",
                },
                completion = {
                    privateEditable = {
                        enable = true;
                    }
                }
            }
        }
    }
end

local ensure_packer = function()
    local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
        vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

vim.g.loaded_python_provider = 0

-- use the system-wide python and the python-neovim package on linux
-- for MacOS the package is user installed
if vim.loop.os_uname().sysname ~= 'Darwin' then
    vim.g.python3_host_prog='/usr/bin/python3'
end

require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- color schemes
    use 'folke/tokyonight.nvim'
    use 'rebelot/kanagawa.nvim'
    use 'NLKNguyen/papercolor-theme'

    -- nvim-tree tends to get lost when:
    -- - opening it while using multiple tabs (e.g. open a file from the second tab).
    -- - opening horizontal/vertical simultaneously (e.g. when using :Ack for search).
    --
    -- use {
    --     'nvim-tree/nvim-tree.lua',
    --     requires = {
    --         'nvim-tree/nvim-web-devicons',
    --     },
    --     config = function()
    --         require('nvim-web-devicons').setup()
    --         require('nvim-tree').setup({
    --             on_attach = function(bufnr)
    --                 local api = require('nvim-tree.api')
    --                 api.config.mappings.default_on_attach(bufnr)
    --                 options = { desc = 'nvim-tree: open on a tab', buffer = bufnr, noremap = true, silent = true, nowait = true }
    --                 vim.keymap.set('n', 't', api.node.open.tab, options)
    --             end
    --         })
    --     end,
    -- }
    use {
        'nvim-neo-tree/neo-tree.nvim',
        requires = {
            'nvim-lua/plenary.nvim',
            'nvim-tree/nvim-web-devicons',
            'MunifTanjim/nui.nvim',
        },
        config = {
            auto_clean_after_session_restore = false, -- Automatically clean up broken neo-tree buffers saved in sessions
        }
    }

    use {
        'nvim-treesitter/nvim-treesitter',
        config = TreeSitterUpdateParsers,
        run = ':TSUpdate'
    }

    use {
        -- this is a hack that allows the StatusLineSetup hook to define all of its
        -- dependencies
        --
        -- note that these plugins are required for the statusline, but are also
        -- useful on their own right
        'hackaugusto/dotfiles',
        requires = {
            'mhinz/vim-signify',
            'jmcantrell/vim-virtualenv',
            'tpope/vim-fugitive',
        },
        config = StatusLineSetup,
    }

    use 'tpope/vim-repeat'
    use 'pgdouyon/vim-evanesco'  -- search for selected text
    use 'junegunn/vim-easy-align'

    use 'wellle/targets.vim'
    use 'tpope/vim-surround'
    use 'justinmk/vim-sneak'
    use {
        'ggandor/leap.nvim',
        config = function()
            require('leap').set_default_keymaps()
            vim.api.nvim_set_hl(0, 'LeapBackdrop', { fg = '#707070' })
        end,
    }

    use 'mileszs/ack.vim'
    use 'Shougo/deoplete.nvim'
    use 'deoplete-plugins/deoplete-jedi'

    use {
        'hrsh7th/nvim-cmp',
        requires = {
            'saadparwaiz1/cmp_luasnip',
            'L3MON4D3/LuaSnip' ,
            'hrsh7th/cmp-nvim-lsp',
            'neovim/nvim-lspconfig',
            'hrsh7th/cmp-cmdline',
            'hrsh7th/cmp-buffer',
        },
        config = CmpSetup,
    }
    use {
        'saecki/crates.nvim',
        event = { "BufRead Cargo.toml" },
        requires = { { 'nvim-lua/plenary.nvim' } },
        config = function() require('crates').setup() end,
    }

    use 'dense-analysis/ale'
    use 'stsewd/isort.nvim'
    use {
        'tpope/vim-endwise',
        ft = {
            'lua', 'sh', 'zsh', 'vim', 'c', 'cpp', 'xdefaults', 'haskell', 'snippets'
        }
    }

    use 'rust-lang/rust.vim'

    use 'davidhalter/jedi-vim'
    use 'vim-scripts/python_match.vim'
    use 'vim-scripts/python.vim' -- block motions

    if packer_bootstrap then
        require('packer').sync()
        vim.api.nvim_command('normal UpdateRemotePlugins()')
    else
        -- these settings required the above plugins, don't do them on the first
        -- run because lots of errors are raised
        -- vim.api.nvim_command('colorscheme tokyonight-moon')
        -- vim.opt.background=light
        vim.api.nvim_command('colorscheme kanagawa')
    end
end)

vim.g.ale_echo_msg_format = '[%linter%] %s [%severity%]'
vim.g.ale_c_parse_compile_commands = 1
vim.g.ale_cpp_clang_options = '-Wall'
vim.g.ale_cpp_gcc_options = '-Wall'
vim.g.ale_fix_on_save = 1
vim.g.ale_fixers = {
    ['python']= {'black', 'isort', 'trim_whitespace', 'remove_trailing_lines', 'yapf'},
    ['rust']= {'remove_trailing_lines', 'trim_whitespace'},
    ['cpp']= {'clang-format', 'clangtidy', 'remove_trailing_lines', 'trim_whitespace', 'uncrustify'},
    ['dart']= {'dartfmt', 'trim_whitespace', 'remove_trailing_lines'},
}
vim.g.ale_rust_cargo_use_clippy = 1
vim.g.ale_linters = {['rust'] = {'rls', 'cargo'}}
vim.g.ale_python_black_options = '--line-length 125'
vim.g.rustfmt_autosave = 1

vim.g['jedi#completions_enabled'] = 0
vim.g['jedi#use_tabs_not_buffers'] = 1
vim.g['jedi#goto_command'] = 'gd'
vim.g['jedi#goto_assignments_command'] = 'ga'
vim.g['jedi#goto_definitions_command'] = ''
vim.g['jedi#usages_command'] = ''
vim.g['jedi#rename_command'] = '<leader>r'
vim.g['jedi#rename_command'] = ''
vim.g['jedi#show_call_signatures'] = 1
vim.g['jedi#show_call_signatures_delay'] = 100
vim.g['jedi#smart_auto_mappings'] = 0

vim.g.signify_sign_add = '+'
vim.g.signify_sign_delete_first_line = '-'
vim.g.signify_sign_change = '~'

vim.g.ackprg = 'ag --vimgrep'

-- mappings
vim.g.mapleader = ' '

-- move just the content
vim.keymap.set('', '<up>', '<c-y>k', {})
vim.keymap.set('', '<down>', '<c-e>j', {})

-- change buffers
vim.keymap.set('n', '<leader>w', ':w<cr>', {noremap = true})
vim.keymap.set('n', '<leader>q', ':q<cr>', {noremap = true})
vim.keymap.set('n', '<leader>n', ':nohl<cr>', {noremap = true})
vim.keymap.set('n', '<leader>p', ':set paste!<cr>', {noremap = true})
-- paste contents of the selection buffer
vim.keymap.set('n', '<leader>P', '"*p', {noremap = true})

vim.keymap.set('n', '<leader>d', ':ALEDetail<CR>', {noremap = true})

-- Up and Down act as ^n and ^p for the autocomplete menu
vim.keymap.set('i', '<expr><Down>', 'pumvisible() ? "<C-n>" : "<Down>"', {noremap = true})
vim.keymap.set('i', '<expr><Up>', 'pumvisible() ? "<C-p>" : "<Up>"', {noremap = true})

-- filetype settings
vim.api.nvim_create_autocmd('BufEnter', {command = 'set completeopt-=preview'})  -- Disable documentation preview

function RemoveOldSwap(filename)
    if vim.fn.getftime(vim.v.swapname) < vim.fn.getftime(filename) then
        vim.v.swapchoice = 'd'
    end
end
vim.api.nvim_create_autocmd('SwapExists', {command = 'call v:lua.RemoveOldSwap(expand("<afile>:p"))'})

vim.api.nvim_create_autocmd("BufRead", {
    group = vim.api.nvim_create_augroup("CmpSourceCargo", { clear = true }),
    pattern = "Cargo.toml",
    callback = function()
        local cmp = require('cmp')
        cmp.setup.buffer({ sources = { { name = "crates" } } })
    end,
})


vim.cmd [[
" autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })

augroup Lisp
    autocmd!
    autocmd FileType lisp set showmatch
augroup END

augroup Vim
    autocmd!
    autocmd FileType vim set shiftwidth=2 softtabstop=2 tabstop=2 expandtab
augroup END

augroup Python
    autocmd!
    autocmd FileType python set nowrap
augroup END
]]
