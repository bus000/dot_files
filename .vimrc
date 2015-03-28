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
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""
" Bundle Manager
""""""""""""""""""

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" Place plugins here.

" The color theme Gruvbox.
Plugin 'morhetz/gruvbox'

" Makes % match tags in html and xml.
Plugin 'matchit.zip'

" Show git diff in the right column.
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

call vundle#end()
filetype plugin indent on

""""""""""""""""""
" General
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
" UI
""""""""""""""""""

" Make vertical bar at 81'st character.
set colorcolumn=81

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

" Set a foldcolumn in left side of screen showing folds.
set foldcolumn=1

" Make vim open files with no fold.
set foldlevel=20

" Make vim not care that a buffer is not saved when opening a new buffer.
set hidden

" Always show statusline.
set laststatus=2

""""""""""""""""""
" Colors and Fonts
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
" Text, Tab and Indent
""""""""""""""""""

" Use spaces like a real man.
set expandtab

" Indent smart when using tab.
set smarttab

" Set tab width to 4.
set shiftwidth=4
set tabstop=4

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Remove trailing space on save.
autocmd BufWritePre * :%s/\s\+$//e

" Set spelling
set spell
set spelllang=en_gb

""""""""""""""""""
" Files, Backup and Undo
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
" Keymappings
""""""""""""""""""

" Map space to go to insert mode.
nnoremap <Space> i

" Map quit to leader e.
nnoremap <leader>e :q<cr>

" Save with leader w.
nmap <leader>w :w!<cr>

" Remember selection when indenting.
vnoremap < <gv
vnoremap > >gv

" Map tagbar to F8.
nnoremap <F8> :TagbarToggle<CR>

" Set zc to close all outer layer folds.
nnoremap zc :%foldc<CR>

" Make enter create new lines without going to insert mode.
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

" Remove search highlighting with leader + n.
nnoremap <leader>n :nohlsearch<CR>

" Split to to windows in same file with leader + s.
nnoremap <leader>s :vsplit<CR>
