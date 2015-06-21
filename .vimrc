""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Maintainer:
"     Magnus Stavngaard - magnus@stavngaard.dk
"
" Sections:
"  -- Bundle Manager
"  -- General
"  -- UI
"  -- Colors and Fonts
"  -- Text, Tab and Indent
"  -- Files, Backup and Undo
"  -- Keymappings
"  -- Sources
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""
" Bundle Manager:
""""""""""""""""""

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" The color theme Gruvbox.
Plugin 'morhetz/gruvbox'

" Makes % match tags in html and xml.
Plugin 'matchit.zip'

" Show git diff in the left column.
Plugin 'airblade/vim-gitgutter'

" A more powerful line at bottom of screen.
Bundle 'Lokaltog/vim-powerline'

" Fuzzy search files.
Plugin 'kien/ctrlp.vim'

" Multiple cursors for vim.
Bundle 'terryma/vim-multiple-cursors'

" Makes vim automatically close parenthesis and quotes.
Bundle 'Raimondi/delimitMate'

" Automatically create tag files.
Bundle 'xolox/vim-easytags'
Bundle 'xolox/vim-misc'

" Jump to definitions in tag file.
Bundle 'majutsushi/tagbar'

" Plugin for outcommenting lines.
Bundle 'scrooloose/nerdcommenter'

" Better use for tab key which now switch functionality based on where it is
" used.
Bundle 'ajh17/VimCompletesMe'

" Surround words, visual selection and more in parenthesis quotes and more.
Plugin 'Surround.vim'

" Add support for repeats of more of the plugin commands.
Plugin 'repeat.vim'

" Easily align of text objects in vim.
Plugin 'junegunn/vim-easy-align'

" Expand a visual selection with + and underscore.
Plugin 'terryma/vim-expand-region'

" Distraction free writing.
Plugin 'junegunn/goyo.vim'

" * and # search forwards and backwards for visual selected text.
Bundle 'bronson/vim-visual-star-search'

call vundle#end()
filetype plugin indent on

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

" Map colon to semicolon.
nmap ; :

" Working dir is same as file
set autochdir

" Set backspace to work like in most other editors.
set backspace=2

" Turn on % matching xml tags.
runtime macros/matchit.vim

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

" Turn on syntax hightlighting.
syntax enable

" Choose color scheme.
colorscheme gruvbox
set background=dark

" Set vim to use 256-color terminal.
set t_Co=256

" Set vim to use utf8.
set encoding=utf8

""""""""""""""""""
" Text, Tab and Indent:
""""""""""""""""""

" Use spaces like a real man.
set expandtab

" Indent smart when using tab.
set smarttab

" Set tab width to 4.
set shiftwidth=4
set tabstop=4

" Remove trailing space on save.
autocmd BufWritePre * :%s/\s\+$//e

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

""""""""""""""""""
" Files, Backup and Undo:
""""""""""""""""""

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
nnoremap <leader>f {!}par -w80<CR>

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
nnoremap <leader>a ggVG

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

" Make vim movement move in lines displayed and not acutal lines.
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" The cousin of the already working delete in paragraph (dip).
nnoremap dp d}

" Fast exiting of multiple files.
nnoremap <leader>q :qa<CR>

""""""""""""""""""
" Sources:
""""""""""""""""""

" https://bitbucket.org/sjl/dotfiles/src

" https://github.com/davidpdrsn/dotfiles
