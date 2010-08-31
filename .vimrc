"r _vimrc -- my vimrc as of 2006
" @Author:      Jose Quesada (mailto:quesada AT gmail.com)
" @Website:     http://josequesada.name
" @License:     GPL: http://www.gnu.org/copyleft/gpl.html
" @Created:     05-Apr-2006
" To use it, copy it t
" for Unix and OS/2:  ~/.vimrc
" for MS- and Win32:  $VIM\_vimrc

" using pathogen to have multiple runtime paths. Each plugin will have his own
" tree. TODO this doesn't work. Produces huge spike on cpu for seconds
"filetype off
"call pathogen#runtime_append_all_bundles()
"filetype plugin indent on

"""""""""""""""""""""""""""""""""""""""""""""""""""
" settings
""""""""""""""""""""""""""""""""""""""""""""""""""""

" I use as many plugins as I can from packages:
" equo install app-vim/eselect-syntax app-vim/conque app-vim/bufferexplorer
" app-vim/align app-vim/autoalign app-vim/colorschemes app-vim/extra-syntax
" app-vim/nerdcommenter app-vim/searchcomplete app-vim/taglist app-vim/vimpython

" plugins not on packages: fuzzyfinder, simplepairs are installed the vim way

"Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.

set textwidth=80			"" 80 columns wide text is good
set t_Co=256 			    "" use 256 colors to maximize compatibility vim-gvim
set nocompatible            "" for advanced vim-only things
set history=50           "keep 50 lines of command line history
set ruler                "show the cursor position all the time
set showcmd              "display incomplete commands
set showmode             "display the mode you are in everytime
set mouse=a  			"			 mouse works in all modes
set incsearch            "do incremental searching
set showmatch            "balancing brackets like in emacs
set nowrap                "linewrapping off
set nospell
set ignorecase           "evident
set nostartofline        "to keep the cursor at the same horizontal location when
" Set up default tab spacing
set expandtab            "Expand <Tab> to spaces in Insert mode
set tabstop=4            "number of spaces that a <Tab> in the file counts for
set laststatus=2         "show status line only if there are more than one windows
set shiftwidth=4         "size of tabs by default
set backup               "backUps!!
set title               "see filename in Xterm title
set noerrorbells 		"Turn off annoying error bells
set novisualbell
set t_vb=
set showcmd             " display incomplete commands
set mousefocus          " agree with focus follows mouse in xmonad
set ttyfast             " Improve smoothness or redraw for newer terminals
set whichwrap+=h,l      " Allow cursor keys to line wrap
set winminheight=0      " Minimal height of a non-current window


" Time out on mappings after two seconds, key codes after a tenth of a second
set timeoutlen=2000
set ttimeoutlen=100

" remove toolbar and menubar in gui
set guioptions-=T
set guioptions-=m
set guicursor+=a:blinkon0 "prevent cursor blinking
set so=5 " electric borders for scroll. 5 lines"

" if you use fish shell
if $SHELL =~ '/usr/bin/fish' | set shell=/bin/sh | endif

"have a red cursor, no matter which color scheme you use
highlight Cursor gui=reverse guifg=red guibg=white

" Soft wrapping text
" http://vimcasts.org/episodes/soft-wrapping-text/
" simply enter :Wrap
command! -nargs=* Wrap set wrap linebreak nolist textwidth=0

" GRB: use emacs-style tab completion when selecting files, etc
set wildmode=longest,full

"setting about old window resizing behavior when open a new window
set winfixheight
" not let all windows keep the same height/width
set noequalalways



let mapleader="," " remap leader
set backupdir =/home/quesada/vimbackup "where to put backup files, default in the same di
setlocal cursorline " current line
" taglist
"If you have multiple tag files (across different projects), or your current working
"directory changes, it is useful to have vim search recursively upwards for the tags file.
set tags=tags;/
set foldmethod=marker
behave mswin " needed to select things with shift-arrows

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Commands and Mappings  {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

 "### setting for fuzzyfinder.vim {{{2
    let g:fuf_modesDisable = []
    let g:fuf_abbrevMap =
                \ { "^,w" : [$PROJECT_DIR],
                \ "^,v" : map(filter(split(&runtimepath, ','), 'v:val !~ "after$"'), 'v:val . ''/**/'''),
                \ "^,r" : ["app/models", "app/views", "app/controllers", "test/functional", "test/integration", "test/unit", "test/fixtures", "db/fixtures"],
                \ "^,u" : [$PROJECT_DIR . "/../ui_design/template/feb2010/html/02 - current/"],
                \ }
    "let g:fuf_abbrevMap = {
          "\ '^vr:' : map(filter(split(&runtimepath, ','), 'v:val !~ "after$"'), 'v:val . ''/**/'''),
          "\ '^m0:' : [ '/mnt/d/0/', '/mnt/j/0/' ],
          "\ }

    let g:fuf_mrufile_maxItem = 300
    let g:fuf_mrucmd_maxItem = 400

    let g:fuf_file_exclude = '\v\~$|\.o$|\.bak$|\.pyc$|\.exe$|\.bak$|\.swp$|\.swo|\.DS_Store$|\.svn/$|\.gitignore$|\.git/$|\CVS/$|((^|[/\\])\.[/\\]$)'

    let g:fuf_ignoreCase = 1
    let g:fuf_useMigemo = 0

    " customize mode's prompts
    let g:fuf_buffer_prompt = '[Buffer]'
    let g:fuf_buffer_promptHighlight = 'BufferMode'
    let g:fuf_file_prompt = '[File]'
    let g:fuf_file_promptHighlight = 'FileMode'
    let g:fuf_dir_prompt = '[Dir]'
    let g:fuf_dir_promptHighlight = 'DirMode'
    let g:fuf_mrufile_prompt = '[MruFile]'
    let g:fuf_mrufile_promptHighlight = 'MruFileMode'
    let g:fuf_mrucmd_prompt = '[MruCmd]'
    let g:fuf_mrucmd_promptHighlight = 'MruCmdMode'
    let g:fuf_bookmark_prompt = '[Bookmark]'
    let g:fuf_bookmark_promptHighlight = 'BookmarkMode'
    let g:fuf_tag_prompt = '[Tag]'
    let g:fuf_tag_promptHighlight = 'TagMode'
    let g:fuf_taggedFile_prompt = '[TaggedFile]'
    let g:fuf_taggedFile_promptHighlight = 'TaggedFileMode'
    let g:fuf_givenFile_prompt = '[GivenFile]'
    let g:fuf_givenFile_promptHighlight = 'GivenFileMode'


    let g:fuf_keyOpenTabpage = '<C-t>'
    let g:fuf_keyOpen = '<CR>'
    let g:fuf_keyOpenSplit = '<C-l>'
    let g:fuf_keyOpenVsplit = '<C-V>'

    :noremap ,fg :FufFile<CR>  "global. starting from /
    :noremap ,ff :FufFileWithCurrentBufferDir<CR>
    :noremap ,fb :FufBuffer<CR>
    :noremap ,fm :FufMruFile<CR>
    :noremap ,f; :FufMruCmd<CR>
    :noremap ,fk :FufBookmark<CR>
    :noremap ,fa :FufAddBookmarkAsSelectedText<CR>
    :noremap ,fd :FufDir<CR>
    :noremap ,ft :FufTaggedFile<CR>
    :noremap ,fg :FufTag<CR>
    :noremap ,f] :FufTag! <C-r>=expand('<cword>')<CR><CR>
    :noremap ,fc :FufRenewCache<CR>
    :noremap ,fi :FuzzyFinderEditInfo<CR>
    :noremap ,fr :call fuf#givenfile#launch('', 0, '> ', split(glob('./**/*'), "\n"))<CR>

    " super find file command, will search the files recursively from current
    " directory
    :noremap <silent> ,fs :call fuf#givenfile#launch('', 0, '[SuperFF]', split(glob("`~/tools/get_file_list.sh`"), "\n"))<CR>

    "let listener = {}
    "function! listener.onComplete(item, method)
      "let content = join(split(a:item, ' ')[1:], ' ')
      "exec "norm i " . content . ''
      ""call setline(line('.'), content)
    "endfunction

    "function! listener.onAbort()
      "echo "Abort"
    "endfunction

    "" Select an item from a given list.
    "if filereadable(expand("~/.mutt/aliases"))
      "let g:aliases_lines = []
      "for a in readfile(expand("~/.mutt/aliases"))
          "let parts = split(a, ' ')
          "call add(g:aliases_lines, join(parts[1:], ' '))
      "endfor
      ":noremap ,fe :call g:FuzzyFinderMode.CallbackItem.launch('', 1, listener, g:aliases_lines, 0)<CR>
    "endif

    " FuzzyFinderTag is really useful especially after we process the tags
    " file, for example generate a tags file for files, that will minic the
    " behavior of TextMate easily. So give it a seperate shortcut
    :noremap ,s :FufTag<CR>

    let g:fuf_previewHeight = 0
    "}}}2

    "### setting for NERD_comments.vim {{{2
    " make NERD_comments silent
    let g:NERDMenuMode=0
    :map <silent> <leader>ct :call NERDComment(1, 'toggle')<CR>
    "### }}}2

    "### setting for taglist.vim {{{2
    nnoremap <silent> <F8> :TlistToggle<CR>
    "### }}}2

"### }}}1

" changing modes, quitting
inoremap <C-q> <esc>:wq!<cr>   " ctrl Q quits
imap jj <Esc>
imap ZZ <esc>:wq!<cr>   " ZZ saves and leaves as well in insert mode

" copy-paste like god intended (cua). Needs xclip installed
" (http://vim.wikia.com/wiki/In_line_copy_and_paste_to_system_clipboard)
vmap <C-c> y:call system("xclip -i -selection clipboard", getreg("\""))<CR>:call system("xclip -i", getreg("\""))<CR>
nmap <C-v> :call setreg("\"",system("xclip -o -selection clipboard"))<CR>p

" groovyness in Insert mode (lets you paste and keep on typing)
" This blows away i_CTRL-V though (see :help i_CTRL-V)
imap <C-v> <Esc><C-v>a

" Swap ; and :  Convenient.
nnoremap ; :
nnoremap : ;

syntax on
set hlsearch
colors desert "zenburn

" avoid losing indentation when hitting scape or pasting
imap <CR> <CR> <BS>
set wildignore=*.o,*.obj,*.bak,*.exe
" configure TOhtml
" NOTE: it interaracts with TSkeleton, it doesn't work for now
let html_use_css =1

" Use CTRL-T for opening new tabs
noremap     <C-T>               :tabnew<CR>
vnoremap    <C-T>               <C-C>:tabnew<CR>
inoremap    <C-T>               <C-O>:tabnew<CR>

" ALTERNATIVE USES OF TAB
" idea from emacs: tab anywhere in the line does esc + V + =
" TODO

" tab completion
" if at the begining of the line, tab, otherwise completion c-p (nice !)
function! InsertTabWrapper()
      let col = col('.') - 1
      if !col || getline('.')[col - 1] !~ '\k'
          return "\<tab>"
      else
          return "\<c-p>"
      endif
endfunction

"then define the appropriate mapping:
inoremap <tab> <c-r>=InsertTabWrapper()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntax highligting prefs. and keyboard shortcuts
""""""""""""""""""""""""""""""""""""""""""""""""""""

"ap <S-tab> :b #<cr>                         " !!alternate buffers


" CTRL R calculates math operations
ino <C-E> <C-O>yiW<End>=<C-R>=<C-R>0<CR>

vmap <silent> g/    y/<C-R>=escape(@", '\\/.*$^~[]')<CR><CR>

"  MAKE IT EASY TO UPDATE/RELOAD_vimrc
if has ("win32")
	map ,s :source /home/quesada/_vimrc<CR>
	map ,v :e /home/quesada/_vimrc<CR>
else
	map ,s :source /home/quesada/.vimrc<CR>
	map ,v :e /home/quesada/.vimrc<CR>
	set backupdir =/home/quesada/vimbackup "where to put backup files, default in the same di
endif


 " ths
 " Use CTRL-D for saving, also in Insert mode
noremap <C-D>           :update<CR>
vnoremap <C-D>          <C-C>:update<CR>
inoremap <C-D>          <C-O>:update<CR>

" TIP to export the syntax highlighting to html, run in command mode:
" :runtime! syntax/2html.vim

" Note abbreviations for time
iab Cdate <C-R>=strftime(" %d.%m.%y - %H")<CR>
iab _lm  <C-R>=" Last Modified: "  . strftime( " %a %b %d %H:%M:%S %Y" )<CR>

" make arrow keys work in visual mode
vnoremap <Left> h
vnoremap <Right> l
vnoremap <Up> k
vnoremap <Down> j


" simulate shift-arrows (select block in windows) with control-arrows
" TODO make it taylor-made to our terminal, this mapping is for putty
inoremap <ESC>[A <C-O>vk
vnoremap <ESC>[A k
inoremap <ESC>[B <C-O>vj
vnoremap <ESC>[B j
inoremap <ESC>[C <C-O>vl
vnoremap <ESC>[C l
inoremap <ESC>[D <C-O>vh
vnoremap <ESC>[D h

" map shift space to esc
imap <S-space> <ESC>



""""""""""""
" important for CUA: behave like windows
""""""""""""
source $VIMRUNTIME/mswin.vim

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" autocomand
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if has("autocmd")
    " dont see ^M again
    autocmd BufRead * silent! %s/[\r \t]\+$//
    set statusline=%<%f\ [%{fugitive#statusline()}]\ %h%m%r%=%-14.(%l,%c%V%)\ %P
   autocmd FileType actionscript setlocal sw=4 sts=4 et
    "if version >= 700
        autocmd InsertEnter * hi StatusLine term=reverse ctermbg=0 gui=undercurl    guisp=orange
        autocmd InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
    "endif


    " I like for vim to change directory to whatever file I'm currently editing.
    " This makes it easier to search for related files in the same directory -
    " and so that :n .<Enter> does what's expected.
    autocmd BufEnter * lcd %:p:h

    " Source the vimrc file after saving it
    autocmd bufwritepost .vimrc source $MYVIMRC

	" Highlight work under cursor
	autocmd CursorMoved * exe printf('match IncSearch /\<%s\>/', expand('<cword>'))

   " Enable file type detection.
   " Use the default filetype settings, so that mail gets 'tw' set to 72,
   " 'cindent' is on in C files, etc.
   " Also load indent files, to automatically do language-dependent indenting.
   filetype plugin indent on

   autocmd BufEnter * :syntax sync fromstart "  ensure every file does syntax hi
   autocmd BufEnter * :lcd %:p:h "  switch to current dir

   " Put these in an autocmd group, so that we can delete them easily.
   augroup vimrcEx
      au!

    " For all text files set 'textwidth' to 78 characters.
    autocmd FileType text setlocal textwidth=80

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif

   augroup END

   " Filetypes
   au BufNewFile,BufRead *.as set filetype=actionscript
   au BufNewFile,BufRead *.r set filetype=r
   au BufNewFile,BufRead *.fish set filetype=sh
   autocmd BufNewFile,BufRead *.json set ft=javascript

   augroup vimFiletypes
   au!
   autocmd FileType actionscript setlocal sw=4 sts=4 et
   autocmd FileType python       setlocal sw=4 sts=4 et tw=72
   autocmd FileType ruby         setlocal sw=2 sts=2 et
   autocmd FileType vim          setlocal sw=4 sts=4 et
   " Start python on F5
   autocmd FileType python map <F5> :w<CR>:!python "%"<CR>

   augroup END

   "to get equally spaced buffer windows independent of the main GVIM window size.
    autocmd VimResized * wincmd =

    " hightlight whitespaces. Important for git and other vcs
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
    autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
    autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
    autocmd BufWinLeave * call clearmatches()

    " Resolving performance problems on the hightlight whitespaces
    autocmd BufWinLeave * call clearmatches()

else
   " Copy indent from current line when starting a new line (typing <CR>
   set autoindent    " always set autoindenting on
endif " has("autocmd")


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugins, new commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-fuzzyfinder plugin
map <Leader>t :FufFile<Enter>

" NERD TREE not used because package fails
"let loaded_nerd_tree=1
"let NERDTreeMouseMode=3 "single-click operation
"let NERDTreeQuitOnOpen=1

" Vim commander
noremap <silent> <F11> :cal VimCommanderToggle()<CR>

" Soft wrapping text
" http://vimcasts.org/episodes/soft-wrapping-text/
" simply enter :Wrap
command! -nargs=* Wrap set wrap linebreak nolist textwidth=0

"-------------------------------------------------------------------------------
" tskeleton support
" makes headers in comments for files *.r *.txt with author, etc
" it also has some code templates a la textmate
"-------------------------------------------------------------------------------
" the function tskeletonincreaseRevisionNumber is suspect of creating trouble
" with otl and others
"    autocmd BufWritePre * call TSkeletonIncreaseRevisionNumber()
let g:tskelUserName			= "Jose Quesada"
let g:tskelUserEmail		= "quesada AT gmail.com"
let g:tskelUserWWW			= "http://josequesada.name"
let g:tskelLicense			= "GPL: http://www.gnu.org/copyleft/gpl.html"


let g:tskelDir = '/home/quesada/.vim/skeletons/' "
autocmd BufNewFile *.r TSkeletonSetup r.r
"autocmd BufNewFile *.otl TSkeletonSetup outline.otl
autocmd BufNewFile *.rb TSkeletonSetup ruby.rb

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" python-specific (http://www.sontek.net/post/Python-with-a-modular-IDE-(Vim).aspx)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" alternative 1: add the sys path to vim
python << EOF
import os
import sys
import vim
for p in sys.path:
    if os.path.isdir(p):
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF

" alterative 2: use pydiction plugin
" load pydict file for autocompletion
" MUST be the absolute path!!!
" if you add new modules, you need to tell pydiction (running the pydiction.py
" with the name of the module)
let g:pydiction_location = '/home/quesada/.vim/pydiction/complete-dict'

" perl
"  better highlighting
let perl_want_scope_in_variables=1
let perl_extended_vars=1
let perl_include_pod=1

"""""""""
"  c highlighting stuff
""""""""""
autocmd FileType c highlight Statement ctermfg=red
autocmd FileType c highlight Comment ctermfg=darkGrey
autocmd FileType c highlight String ctermfg=green
""""""""""
" vim highlighting Stuff
""""""""""
highlight lineNr ctermfg=black
highlight vimCommand ctermfg=red
"highlight vimComment ctermfg=darkGrey
highlight vimOper ctermfg=black


