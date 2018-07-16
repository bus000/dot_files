""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer:
"     Magnus Stavngaard - magnus@stavngaard.dk
"
" Sections:
"  -- Plugin Manager
"  -- General
"  -- UI
"  -- Colors and Fonts
"  -- Text, Tab and Indent
"  -- Files, Backup and Undo
"  -- Keymappings
"  -- Sources
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""
" Plugin Manager:
""""""""""""""""""

call plug#begin('~/.local/share/nvim/plugged')

" Github color scheme.
Plug 'https://github.com/endel/vim-github-colorscheme'

" Gruvbox color scheme.
Plug 'https://github.com/morhetz/gruvbox'

" Show git diff in the left column.
Plug 'https://github.com/airblade/vim-gitgutter'

" A more powerful line at bottom of screen.
Plug 'https://github.com/vim-airline/vim-airline'

" Multiple cursors for vim.
Plug 'https://github.com/terryma/vim-multiple-cursors'

" Makes vim automatically close parenthesis and quotes.
Plug 'https://github.com/Raimondi/delimitMate'

" Plugin for outcommenting lines.
Plug 'https://github.com/scrooloose/nerdcommenter'
 
" Better use for tab key which now switch functionality based on where it is
" used.
Plug 'https://github.com/ajh17/VimCompletesMe'

" Surround words, visual selection and more in parenthesis quotes and more.
Plug 'https://github.com/tpope/vim-surround'

" Add support for repeats of more of the plugin commands.
Plug 'https://github.com/tpope/vim-repeat'

" Sort haskell import when saving Haskell file.
Plug 'https://github.com/itchyny/vim-haskell-sort-import'

" Vim Language Server.
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

call plug#end()

""""""""""""""""""
" General:
""""""""""""""""""

" Set the history length.
set history=1000

" Read a file again if it is changed from the outside.
set autoread

" Set the leader button for more commands.
let mapleader = ","
let g:mapleader = ","

" Working dir is same as file
set autochdir

" Set backspace to work like in most other editors.
set backspace=2

" Turn on % matching xml tags.
runtime macros/matchit.vim

" Reload files when they change on disc.
set autoread

" Set tags file to be located one folder up in the tree.
set tags=./tags
let g:easytags_dynamic_files = 1

" Set up language servers.
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp'],
    \ 'python': ['pyls'],
    \ }

" Automatically start language servers.
let g:LanguageClient_autoStart = 1

""""""""""""""""""
" UI:
""""""""""""""""""

" Show 10 lines under and over the cursor when scrolling.
set so=10

" Enables the vim ruler.
set ruler

" Turn on the wild menu, the menu with suggestions at the button.
set wildmenu

" Ignore upper and lower case on search.
set ignorecase

" Do not ignore case if uppercase letters is typed.
set smartcase

" Highlight search results.
set hlsearch

" Makes search highlight resuls while searching.
set incsearch

" Highlight the line with the cursor.
set cursorline

" Better regular expressions.
set magic

" Show a matching bracket when curser is over a bracket.
set showmatch

" Tenths of second to blink when matching brackets.
set mat=2

" Turn of sounds on error.
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Change fold to consider lines with equal indent as a fold block.
set foldmethod=syntax

" Remove foldcolumn from the left hand side.
set foldcolumn=0

" Make vim open files with no fold.
set foldlevel=20

" Make vim not care that a buffer is not saved when opening a new buffer.
set hidden

" Always show statusline.
set laststatus=2

" Set the line numbers to be relative to make moving around faster.
set number
set relativenumber

" Ignore compiled files.
set wildignore=*.o,*~,*.pyc

" Don't draw the screen in the middle of executing macros.
set lazyredraw

" Don't syntax highlight long lines.
set synmaxcol=400

""""""""""""""""""
" Colors and Fonts:
""""""""""""""""""

" Enable colors.
set termguicolors

" Set the actual color scheme.
colorscheme gruvbox

" Choose background color dark/light.
set background=dark

" Turn on syntax hightlighting.
syntax enable

" Set vim to use utf8.
set encoding=utf8

""""""""""""""""""
" Text, Tab and Indent:
""""""""""""""""""

" Use spaces like a real man.
set expandtab

" Indent smart when using tab.
set smarttab

" Set tab width to 8 but indent with 4 spaces when tab is pressed.
set shiftwidth=4
set tabstop=8
set softtabstop=4
set shiftround

" Highlight long lines.
"augroup highlightingLongLines
  "autocmd!
  autocmd FileType *        match ErrorMsg '\%>80v.\+'
  autocmd FileType calendar match none
"augroup END

" Set spelling.
set spell
set spelllang=en_gb

" Make vim show tab characters.
set listchars=trail:·,tab:»·
set list

" Don't indent the case in the switch statement in C.
set cinoptions=:0

" Sort Haskell import automatically in .hs files.
autocmd BufWritePre <buffer> HaskellSortImport

""""""""""""""""""
" Files, Backup and Undo:
""""""""""""""""""

" Make easitags update tags file asynchronous and not block vim.
let g:easytags_async = 1

" Turn backup off.
set nobackup
set nowb
set noswapfile

" Make vim detect the type of the file it is opening.
filetype detect

" Keep a history of 1000 undos.
set undolevels=1000

""""""""""""""""""
" Keymappings:
""""""""""""""""""

" Map space to go to insert mode.
nnoremap <Space> i

" Map quit to leader e.
nnoremap <leader>e :q<cr>

" Save with leader w.
nnoremap <leader>w :w!<cr>

" Map tagbar to F8.
nnoremap <F8> :TagbarToggle<CR>

" Set zc to close all outer layer folds.
nnoremap zc :%foldc<CR>

" Make enter create new lines without going to insert mode.
nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>

" Remove search highlighting with leader + n.
nnoremap <silent> <leader>n :nohlsearch<CR>

" Split to to windows in same file with leader + s.
nnoremap <leader>s :vsplit<CR>

" Go to terminal with leader + t.
nnoremap <leader>t :shell<CR>

" Do leader + f to reformat a paragraph.
nnoremap <leader>f mm{!}par -w80<CR>`m

" Make capital Y yank the rest of the line.
nnoremap Y y$

" Resize windows with the arrow keys
nnoremap <up>    <C-W>+
nnoremap <down>  <C-W>-
nnoremap <left>  3<C-W>>
nnoremap <right> 3<C-W><

" Use leader + ev to edit .vimrc file and leader + sv to reload the .vimrc
" file.
noremap <leader>ev :tabedit $MYVIMRC<CR>
noremap <leader>sv :source $MYVIMRC<CR>:e<CR>

" Save vim session and reopen with vim -S.
nnoremap <leader>m :mksession<CR>

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Mapping for toggling Goyo mode.
nnoremap <leader>g :Goyo<CR>

" Select all text as visual.
nnoremap <leader>a maggVG

" Use q to reference quotes.
nnoremap viq vi"
nnoremap vaq va"
nnoremap diq di"
nnoremap daq da"
nnoremap ciq ci"
nnoremap caq ca"

" Use backspace to delete characters in normal mode.
nnoremap <BS> i<BS><Esc>l

" Easier copy pasting to system clipboard.
nnoremap <leader>p "+p
vnoremap <leader>p "+p
vnoremap <leader>c "+y
nnoremap <leader>ciw "+yiw
nnoremap <leader>caw "+yaw

nnoremap <leader>ciq "+yi"
nnoremap <leader>caq "+ya"

nnoremap <leader>cip "+yip
nnoremap <leader>cap "+yap

nnoremap <leader>cib "+cib
nnoremap <leader>cab "+cab

nnoremap <leader>ci) "+ci)
nnoremap <leader>ca) "+ca)
nnoremap <leader>ci( "+ci(
nnoremap <leader>ca( "+ca(

nnoremap <leader>ci{ "+ci{
nnoremap <leader>ca{ "+ca{
nnoremap <leader>ci} "+ci}
nnoremap <leader>ca} "+ca}

nnoremap <leader>ci[ "+ci[
nnoremap <leader>ca[ "+ca[
nnoremap <leader>ci] "+ci]
nnoremap <leader>ca] "+ca]

" Mapping for common latex text modifiers.
inoremap <c-t> \texttt{}<Esc>i
inoremap <c-b> \textbf{}<Esc>i
inoremap <c-f> \textit{}<Esc>i

" Spell correct current word.
inoremap <c-z> <esc><leader>zea

" Mapping to make the word just typed upper case in insert mode.
inoremap <C-u> <esc>mzgUiw`za

" Use ctrl backspace to delete a whole word in insert mode.
imap <C-BS> <C-W>

" Go to normal mode with jk in insert mode and visual mode.
inoremap jk <esc>
vnoremap jk <esc>

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Remember selection when indenting.
vnoremap < <gv
vnoremap > >gv

" Visual selection in tags with big V is selecting by line.
nnoremap Vit vitVkoj
nnoremap Vat vatV
nnoremap Vab vabV
nnoremap VaB vaBV

" Select in $ for latex math mode.
nnoremap vi$ T$vt$

" Make vim movement move in lines displayed and not actual lines.
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" The cousin of the already working delete in paragraph (dip).
nnoremap dp d}

" Fast exiting of multiple files.
nnoremap <leader>q :qa<CR>

" Map colon to semicolon and reversed.
nnoremap ; :
nnoremap : ;

" Start an equation in latex.
inoremap <C-e> <esc><CR>o\begin{equation}<CR>\end{equation}<CR><esc>kko

" Display type of Haskell subexpression.
nnoremap <C-t> :GhcModType<CR>

" Compute the result of the current latex mathematical expression selected.
vnoremap <leader>e "1y:!latcal "<C-R>1"<CR>

" Language server mappings.
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

tnoremap jk <C-\><C-n>
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

""""""""""""""""""
" Sources:
""""""""""""""""""

" https://bitbucket.org/sjl/dotfiles/src


" https://github.com/davidpdrsn/dotfiles
