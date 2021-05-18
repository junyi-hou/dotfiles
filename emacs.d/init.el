;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq message-log-max 10000)

(setq visible-bell t
      ring-bell-function 'ignore
      server-client-instructions nil)

(setq max-mini-window-height 1)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

(defun gatsby:core--unkillable-scratch ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions #'gatsby:core--unkillable-scratch)

(defun gatsby:core--unkillable-message ()
  (if (string= (buffer-name (current-buffer)) "*Messages*")
      (progn
        (delete-window (get-buffer-window "*Messages*"))
        nil)
    t))

(add-hook 'kill-buffer-query-functions #'gatsby:core--unkillable-message)

(defun gatsby:core-refresh-message-buffer ()
  "Clear `*Message*' buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)

(defun gatsby:core--split-vertical (window)
  "Return t if should split WINDOW vertically, otherwise return nil."
  (let* ((h (window-total-height window))
         (w (window-total-width window))
         (ratio (/ (float h) w)))
    (cond
     ((< ratio 0.15) t)
     ((< (/ (float w) 2) 90) nil)
     (t t))))

(defun gatsby:core-split-window (&optional window)
  "Split WINDOW side-by-side, if WINDOW width < 90, split it top-and-down."
  (let ((window (or window (selected-window))))
    (if (gatsby:core--split-vertical window)
        (split-window-right)
      (split-window-below))))

(setq split-window-preferred-function 'gatsby:core-split-window)

(setq scroll-step 1)

(setq straight-repository-branch "develop"
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'https)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package dash)
(use-package f)
(use-package s)

(use-package gcmh :demand t)

(setq gcmh-high-cons-threshold 33554432)

(defun gatsby:core--reset-gc ()
  (setq gc-cons-threshold 25165824))

(add-hook 'after-init-hook #'gatsby:core--reset-gc)

(gcmh-mode 1)

(use-package general :demand t)

(defun gatsby:lisp--fix-indent ()
  (setq-local lisp-indent-function #'gatsby:lisp-indent-function))

(defun gatsby:lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook #'gatsby:lisp--fix-indent)

(use-package no-littering)

(setq auto-save-file-name-transforms  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))
      custom-file (no-littering-expand-etc-file-name "custom.el"))

(load custom-file 'noerror)

(use-package saveplace :straight (:type built-in) :hook (after-init . save-place-mode))

(setq save-place-file (expand-file-name "save-place.el" no-littering-var-directory))

(use-package paren :hook (after-init . show-paren-mode))

(use-package subword :hook (after-init . global-subword-mode))

(use-package simple :straight (:type built-in))

(add-hook 'after-init-hook #'global-visual-line-mode)

(setq visual-fill-column-inhibit-sensible-window-split t)
(use-package visual-fill-column)

(add-hook 'text-mode-hook #'visual-fill-column-mode)

(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook #'auto-fill-mode)

(setq-default fill-column 80)
(setq fill-column 80)

(general-define-key :keymaps 'messages-buffer-mode-map :states 'normal
  "q" #'delete-window)

(general-define-key :keymaps 'messages-buffer-mode-map :states 'normal :prefix "C-c"
  "C-l" #'gatsby:core-refresh-message-buffer)

(use-package whitespace :straight (:type built-in) :hook (before-save . whitespace-cleanup))

(use-package autorevert :hook (after-init . global-auto-revert-mode))

(use-package alert :commands alert)

(setq alert-default-style (if (eq system-type 'gnu/linux) 'libnotify 'osx-notifier))

(setq alert-fade-time 60)

(use-package lab-themes)
(setq custom-safe-themes t)
(lab-themes-load-style 'dark)

(setq-default fringe-indicator-alist
              '((continuation nil nil)
                (truncation nil nil)
                (overlay-arrow . nil)
                (up . nil)
                (down . nil)
                (top nil nil)
                (bottom nil nil nil nil)
                (top-bottom nil nil nil nil)
                (empty-line . nil)
                (unknown . nil)))

(defvar gatsby:right-mode-line '((:eval (gatsby:mode-line-vc-info)) mode-name))

(defun gatsby:format-right-mode-line ()
  (format-mode-line gatsby:right-mode-line))

(defun gatsby:mode-line-vc-info ()
  (if (and vc-mode buffer-file-name)
      (let* ((backend (vc-backend buffer-file-name))
             (repo (file-name-nondirectory (directory-file-name (vc-root-dir))))
             (state (vc-state buffer-file-name backend))
             (str (format "%s:%s "
                          repo
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))
        (propertize (if (> (length str) 25)
                        (concat
                         (substring str 0 (- 25 3))
                         "...")
                      str)
                    'mouse-face 'mode-line-highlight))
    ""))

(defun gatsby:mode-line-maybe-shorten-buffer-name ()
  "Shorten the buffer name if it is too long, otherwise return the original buffer name string."
  (if-let* ((buf-name (buffer-name))
            (str-len (length buf-name))
            (_ (> str-len 50)))
      (format "%s...%s"
              (substring buf-name 0 10)
              (substring buf-name (- str-len 37) str-len))
    buf-name))

(setq mode-line-format
      (list mode-line-mule-info
            mode-line-modified
            mode-line-remote " "
            "%[" '(:eval (gatsby:mode-line-maybe-shorten-buffer-name)) "%]"
            '(:eval (propertize
                     " " 'display
                     `((space :align-to (- (+ right right-fringe right-margin)
                                           ,(+ 3 (string-width (gatsby:format-right-mode-line))))))))
            '(:eval (gatsby:format-right-mode-line))))

(setq-default mode-line-format
              (list mode-line-mule-info
                    mode-line-modified
                    mode-line-remote " "
                    "%[" '(:eval (gatsby:mode-line-maybe-shorten-buffer-name)) "%]"
                    '(:eval (propertize
                             " " 'display
                             `((space :align-to (- (+ right right-fringe right-margin)
                                                   ,(+ 3 (string-width (gatsby:format-right-mode-line))))))))
                    '(:eval (gatsby:format-right-mode-line))))

(use-package ligature :straight (ligature :host github :repo "mickeynp/ligature.el"))

(ligature-set-ligatures
 'prog-mode
 '("!!" "!=" "!==" "!!!" "!≡" "!≡≡" "!>" "!=<" "#(" "#_" "#{" "#?"
   "#>" "##" "#_(" "%=" "%>" "%>%" "%<%" "&%" "&&" "&*" "&+" "&-" "&/"
   "&=" "&&&" "&>" "$>" "***" "*=" "*/" "*>" "++" "+++" "+=" "+>"
   "++=" "--" "-<" "-<<" "-=" "->" "->>" "---" "-->" "-+-" "-\\/" "-|>"
   "-<|" ".."  "..." "..<" ".>" ".~" ".=" "/*" "//" "/>" "/=" "/=="
   "///" "/**" ":::" "::" ":=" ":≡" ":>" ":=>" ":(" ":>:" ":<:" "<$>"
   "<*" "<*>" "<+>" "<-" "<<" "<<<" "<<=" "<=" "<=>" "<>" "<|>" "<<-"
   "<|" "<=<" "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>"
   "<&>" "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<!"
   "<@" "<#" "<%" "<^" "<&" "<?" "<." "</" "<\\" "<\"" "<:" "<->"
   "<!--" "<--" "<~<" "<==>" "<|-" "<<|" "<-<" "<-->" "<<==" "<=="
   "=<<" "==" "===" "==>" "=>" "=~" "=>>" "=/=" "=~=" "==>>" "≡≡"
   "≡≡≡" "≡:≡" ">-" ">=" ">>" ">>-" ">>=" ">>>" ">=>" ">>^" ">>|"
   ">!=" ">->" "??" "?~" "?=" "?>" "???"  "?."  "^=" "^." "^?" "^.."
   "^<<" "^>>" "^>" "\\\\" "\\>" "\\/-" "@>" "|=" "||" "|>" "|||" "|+|"
   "|->" "|-->" "|=>" "|==>" "|>-" "|<<" "||>" "|>>" "|-" "||-" "~="
   "~>" "~~>" "~>>" "[[" "]]" "\">" "_|_"))

(add-hook 'after-init-hook #'global-ligature-mode)

(defconst gatsby:base-font-size 135 "The base level of font size.")

(set-face-attribute
 'default nil
 :family "Monospace"
 :width 'normal
 :weight 'Regular)

(defun gatsby:theme-fontsize-up (&optional size)
  "Increase the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (+ (or size 5) current-size)))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun gatsby:theme-fontsize-down (&optional size)
  "Decrease the font size in the current frame by SIZE.  If SIZE is nil, default to 5."
  (interactive)
  (let* ((current-size (plist-get (custom-face-attributes-get 'default nil) :height))
         (new-size (- current-size (or size 5))))
    (set-face-attribute
     'default (selected-frame)
     :height new-size)))

(defun gatsby:theme--determine-font-size (frame)
  "Determine the fontsize by looking at `x-display-pixel-height'."
  (set-face-attribute
   'default frame
   :height (round (* gatsby:base-font-size (/ (x-display-pixel-height) 1080.0)))))

(add-to-list 'after-make-frame-functions #'gatsby:theme--determine-font-size)

(general-define-key :keymaps '(motion normal visual emacs insert)
  "C-+" 'gatsby:theme-fontsize-up
  "C--" 'gatsby:theme-fontsize-down)

(set-face-attribute 'minibuffer-prompt nil :weight 'normal)

(setq-default indent-tabs-mode nil
              tab-width 4
              electric-indent-inhibit t)

(setq backward-delete-char-untabify-method 'all)

(use-package highlight-indent-guides

  :hook
  (prog-mode . highlight-indent-guides-mode)

  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive nil))

(use-package beacon :straight (beacon :host github :repo "junyi-hou/beacon") :hook (after-init . beacon-mode))

(setq beacon-blink-when-window-scrolls nil
      beacon-can-go-backwards t
      beacon-size 15)

(use-package hl-todo :hook (after-init . global-hl-todo-mode))

(setq hl-todo-keyword-faces '(("TODO" . "#FB4934")
                              ("FIXME"  . "#FB4934")
                              ("NOTE"   . "#FABD2F")
                              ("HACK"   . "#FABD2F")))

(setq hl-todo-exclude-modes nil)

(use-package display-line-numbers :hook (prog-mode . display-line-numbers-mode))

(defun gatsby:theme--set-line-number-background (&rest _)
  (set-face-attribute 'line-number nil :background (face-background 'default)))

(gatsby:theme--set-line-number-background)
(advice-add #'load-theme :after #'gatsby:theme--set-line-number-background)

(setq display-line-numbers-width-start t)

(use-package eldoc-box
  :hook ((text-mode prog-mode) . eldoc-box-hover-mode))

(defun gatsby:eldoc-box--position (width height)
  "Display `eldoc-box' in the bottom right corner of the `selected-window'."
  (let* ((edge (window-pixel-edges))
         (modeline-height (- (nth 3 (window-pixel-edges))
                             (nth 3 (window-inside-pixel-edges))))
         (y (- (nth 3 edge) 5 height modeline-height))
         (x (- (nth 2 edge) 5 width)))
    (cons x y)))
(setq eldoc-box-position-function #'gatsby:eldoc-box--position)

(setq eldoc-box-cleanup-interval 0.5
      eldoc-box-clear-with-C-g t)

(setq eldoc-box-max-pixel-height 400)

(defun gatsby:eldoc-box--set-font-size (&rest _)
  (set-face-attribute
   'default eldoc-box--frame
   :height (face-attribute 'default :height (frame-parameter eldoc-box--frame 'parent-frame))))

(add-hook 'eldoc-box-frame-hook #'gatsby:eldoc-box--set-font-size)

(use-package evil

  :custom (evil-undo-system 'undo-redo)

  :init (evil-mode 1)

  :general
  (:keymaps '(motion normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "H" 'evil-first-non-blank-of-visual-line
   "L" 'evil-end-of-visual-line

   "SPC" nil)

  (:keymaps '(motion normal visual emacs insert)
   "C-h" 'windmove-left
   "C-j" 'windmove-down
   "C-k" 'windmove-up
   "C-l" 'windmove-right

   "C-u" 'evil-scroll-up
   "C-d" 'evil-scroll-down

   "C-e" (lambda () (interactive) (evil-scroll-line-down 5))
   "C-y" (lambda () (interactive) (evil-scroll-line-up 5)))

  (:keymaps '(motion normal visual)
   :prefix "SPC"

   ;; execute
   "ee" 'execute-extended-command
   "el" 'eval-last-sexp
   "eL" (lambda () (interactive)
          (eval-buffer)
          (message "buffer %s evaluated!" (buffer-name)))

   ;; basic function
   "w" 'evil-write
   "k" 'delete-window
   "K" 'delete-frame

   ;; split
   "\\"  (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1))
   "-"   (lambda () (interactive) (evil-window-split) (evil-window-down 1)))

  (:keymaps 'visual
   :prefix "SPC"
   "a" 'align-regexp)

  (:keymaps '(normal motion)
   "<tab>" 'evil-jump-item))

(use-package evil-surround :hook (after-init . global-evil-surround-mode))

(use-package evil-nerd-commenter

  :after evil

  :commands evilnc-comment-or-uncomment-lines

  :general
  (:keymaps '(normal visual)
   :prefix "SPC"
   "t" 'evilnc-comment-or-uncomment-lines))

(use-package expand-region

  :general
  (:keymaps 'visual
   "v" 'er/expand-region
   "V" 'er/contract-region))

(use-package elec-pair :hook (after-init . electric-pair-mode))

(setq-default mode-line-format `((:evil evil-mode-line-tag) ,@mode-line-format))

(--each (buffer-list)
  (with-current-buffer it
    (setq mode-line-format `((:evil evil-mode-line-tag) ,@mode-line-format))))

(evil-define-motion gatsby:evil-next-three-lines ()
  (interactive)
  (evil-next-visual-line 3))

(evil-define-motion gatsby:evil-previous-three-lines ()
  (interactive)
  (evil-previous-visual-line 3))

(general-define-key :keymaps '(motion normal visual)
  "J" 'gatsby:evil-next-three-lines
  "K" 'gatsby:evil-previous-three-lines)

(defun gatsby:evil-visual-tab ()
  "Indent region if in visual-line-mode, otherwise select contains inside a pair of tags via `evil-jump-item'"
  (interactive)
  (if (eq evil-visual-selection 'line)
      (indent-region (region-beginning) (region-end))
    (evil-jump-item)))

(general-define-key :keymaps 'visual
  "<tab>" 'gatsby:evil-visual-tab)

(defun gatsby:evil-normal-state-if-not-motion ()
    "Switch to evil normal state if the current state is not motion state."
    (interactive)
    (unless (or (evil-motion-state-p)
                (evil-normal-state-p)
                (minibufferp (current-buffer))
                (memq major-mode evil-emacs-state-modes))
      (evil-normal-state)))

(global-set-key (kbd "<escape>") #'gatsby:evil-normal-state-if-not-motion)

(global-set-key (kbd "M-u") #'universal-argument)

(defun gatsby:evil-better-newline (newline-fun &rest args)
    "When calling `newline', check whether current line is a comment line (i.e., start with 0 or more spaces followed by `comment-start-skip')  If so, automatically indent and insert `comment-start-skip' after calling `newline' for the first call.  Delete the auto-inserted comment for the second call.  Otherwise call `newline' as default."
    (let* (;; line - the current line as string
           (line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           ;; only-comment - t if the current line starts with comment
           (only-comment (and comment-start-skip
                              (string-match (concat "\\(^[\t ]*\\)\\(" comment-start-skip "\\)") line)))
           ;; newline-string - string insert into newline
           (newline-string (if only-comment
                               (match-string 2 line)
                             "")))
      (if (and only-comment
               (eq last-command 'newline))
          (progn
            (delete-region (line-beginning-position) (point))
            (insert (match-string 1 line)))
        (apply newline-fun args)
        (insert newline-string))))

(advice-add 'newline :around #'gatsby:evil-better-newline)

(defun gatsby:evil--recenter-after-goto-point-max (count)
  "Thin wrapper around `evil-scroll-line-to-center' so center the end-of-buffer after a G motion."
  (unless count
    (recenter nil)))

(advice-add #'evil-goto-line :after #'gatsby:evil--recenter-after-goto-point-max)

(defun gatsby:kill-buffer-and-or-window ()
  "Delete window and kill buffer if `buffer-file-name' is nil."
  (interactive)
  (if (buffer-file-name)
      (kill-buffer)
    (kill-buffer-and-window)))

(general-define-key :keymaps '(normal motion visual) :prefix "SPC"
  "q" #'gatsby:kill-buffer-and-or-window)

(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :defines (selectrum-minibuffer-bindings selectrum-should-sort)
  :custom (selectrum-fix-vertical-window-height t)
  :init (selectrum-mode 1)
  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "oo" #'find-file
   "om" (lambda () (interactive)
          (switch-to-buffer-other-window (get-buffer-create "*Messages*"))))

  ;; evil like scrolling
  (:keymaps 'selectrum-minibuffer-map
   "C-d" #'selectrum-next-page
   "C-u" #'selectrum-previous-page
   "C-e" #'selectrum-next-page
   "C-y" #'selectrum-previous-page))

(defun gatsby:selectrum--remove-base (dir)
  "Insert the string of trimming the base of DIR into the minibuffer."
  (delete-minibuffer-contents)
  (insert (string-trim-right (if (string= dir "~/") (expand-file-name "~/") dir) "[^/]+/?")))

(defun gatsby:selectrum-better-backspace ()
  "If `point' is at \"/\", delete till the last \"/\"."
  (interactive)
  (if (thing-at-point-looking-at "/")
      (let ((dir (minibuffer-contents-no-properties)))
        (gatsby:selectrum--remove-base dir))
    (call-interactively #'backward-delete-char)))

(general-define-key :keymaps #'selectrum-minibuffer-map
  "<backspace>" #'gatsby:selectrum-better-backspace)

(defun gatsby:selectrum-next-candidate-cycle ()
  "Move selection to next candidate, if at the end, go to the top."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (if (= selectrum--current-candidate-index (1- (length selectrum--refined-candidates)))
              (if (selectrum--match-strictly-required-p) 0 -1)
            (1+ selectrum--current-candidate-index)))))

(defun gatsby:selectrum-previous-candidate-cycle ()
  "Move selection to previous candidate, if at the beginning, go to the end."
  (interactive)
  (when selectrum--current-candidate-index
    (setq selectrum--current-candidate-index
          (if (= selectrum--current-candidate-index (if (selectrum--match-strictly-required-p) 0 -1))
              (1- (length selectrum--refined-candidates))
            (1- selectrum--current-candidate-index)))))

(general-define-key :keymaps #'selectrum-minibuffer-map
 "M-j" #'gatsby:selectrum-next-candidate-cycle
 "M-k" #'gatsby:selectrum-previous-candidate-cycle)

(defun gatsby:selectrum-select-current-candidate-if-not-dir ()
  "Select the current candidate. If, however, the current selection is a directory, enter the directory instead of opening it using `dired'."
  (interactive)
  (let*  ((input (minibuffer-contents-no-properties))
          (index selectrum--current-candidate-index)
          (candidate (selectrum--get-candidate index))
          (dir (if (directory-name-p input) input (file-name-directory input))))
    (if (and dir (directory-name-p candidate))
        (progn
          (delete-minibuffer-contents)
          (insert (format "%s%s" dir candidate)))
      (selectrum-select-current-candidate))))

(defun gatsby:selectrum-unified-tab ()
  "<tab> does the following things
1. if there is a common part among candidates, complete the common part;
2. if there is only one candidate, select the candidate
3. if the last command is `gatsby:selectrum-unified-tab', or `selectrum--current-candidate-index' is not 0/-1 (the top candidate), then select the current candidate"
  (interactive)
  (when selectrum--current-candidate-index
    (let* ((common (try-completion "" selectrum--refined-candidates)))
      (cond
       ;; case 3
       ((or (eq last-command this-command)
            (not (memq selectrum--current-candidate-index '(0 -1))))
        (gatsby:selectrum-select-current-candidate-if-not-dir))
       ;; case 2
       ((= 1 (length selectrum--refined-candidates))
        (gatsby:selectrum-select-current-candidate-if-not-dir))
       ;; case 1
       ((not (string= common ""))
        (let*  ((input (minibuffer-contents-no-properties))
                (dir (if (directory-name-p input) input (file-name-directory input))))
          (if dir
              (progn
                (delete-minibuffer-contents)
                (insert (format "%s%s" dir common)))
            (insert common))))))))

(general-define-key :keymaps 'selectrum-minibuffer-map
  "<tab>" #'gatsby:selectrum-unified-tab)

(general-define-key :keymaps '(motion normal visual emacs insert)
  "<C-return>" #'selectrum-repeat)

(defcustom selectrum-become-command-context-list
  '((helpful-variable helpful-function)
    (consult-outline consult-line consult-flymake)
    (find-file switch-to-buffer project-plus-switch-project
               project-plus-find-file consult-buffer))
  "The selectrum-become context list.

    Each element is a list of selectrum commands that should be considered
    within the same group and can become each other. See the default value
    for example."
  :type '(list '(list command))
  :group 'selectrum)

(defun selectrum-become ()
  (interactive)
  (let ((cmd selectrum--last-command)
        (input selectrum--previous-input-string)
        (enable-recursive-minibuffers t))
    (let ((new-cmd (completing-read "Select different command: "
                                    (->> selectrum-become-command-context-list
                                      (--first (memq cmd it))
                                      (--map (symbol-name it))
                                      (--filter (not (eq (symbol-name cmd) it)))))))
      (run-at-time 0 nil
                   (lambda ()
                     (let ((selectrum--repeat t)
                           (selectrum--previous-input-string input))
                       (call-interactively (intern new-cmd))
                       (setq selectrum--current-candidate-index 0
                             selectrum--last-command (intern new-cmd)))))
      (abort-recursive-edit))))

(general-define-key :keymaps #'selectrum-minibuffer-map
  "<C-return>" #'selectrum-become)

(use-package prescient

  :config
  (setq prescient-save-file (concat no-littering-var-directory "prescient-save.el"))
  (prescient-persist-mode 1))

(use-package selectrum-prescient :config (selectrum-prescient-mode 1))

(use-package consult
  :commands
  (consult--read)
  :custom
  (consult-preview-key nil)
  :general
  (:keymaps '(motion normal visual)
   :prefix "SPC"
   "ob" #'consult-buffer))

(defun gatsby:consult-outline (&optional initial)
  "Override `consult-outline' to enable optional initial INPUT."
  (interactive)
  (let ((cands (consult--with-increased-gc (consult--outline-candidates))))
    (consult--read
     cands
     :prompt "Go to heading: "
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :add-history (thing-at-point 'symbol)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :initial initial
     :state (consult--jump-state))))

(advice-add #'consult-outline :override #'gatsby:consult-outline)

(defun gatsby:consult-search-visual-line (beg end)
  (interactive "r")
  (evil-exit-visual-state)
  (consult-line (buffer-substring-no-properties beg end)))

(defun gatsby:consult-search-visual-outline (beg end)
  (interactive "r")
  (evil-exit-visual-state)
  (consult-outline (buffer-substring-no-properties beg end)))

(general-define-key :keymaps '(motion normal)
  "*" #'consult-line
  "#" #'consult-outline)

(general-define-key :keymaps 'visual
  "*" #'gatsby:consult-search-visual-line
  "#" #'gatsby:consult-search-visual-outline)

(setq evil-search-module 'isearch)

(defun gatsby:consult-line-from-evil (arg)
  "Take current search string and run `consult-line' on it.
If ARG is non-nil, run `consult-outline' instead."
  (interactive "P")
  (let ((str isearch-string)
        (enable-recursive-minibuffers t)
        (fn (if arg #'consult-outline #'consult-line)))
    (run-at-time 0 nil fn str)
    (abort-recursive-edit)))

(general-define-key :keymaps 'isearch-mode-map
  "<C-return>" #'gatsby:consult-line-from-evil)

(use-package consult-selectrum
  :straight (:type built-in)
  :after (consult selectrum))

(use-package marginalia :straight (marginalia :host github :repo "minad/marginalia" :branch "main"))
(marginalia-mode 1)

(use-package eshell)

(setenv "PAGER" "cat")

(setenv "TERM" "xterm-256color")

(use-package xterm-color)

(add-hook 'eshell-mode-hook #'company-mode)

(defun gatsby:eshell--setup ()
  "Further setup eshell mode."
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 10000)
  (require 'esh-mode)
  (add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq eshell-buffer-maximum-lines 12000)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-list-files-after-cd t)
  (setq eshell-error-if-no-glob t
        eshell-glob-case-insensitive t)
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-scroll-to-bottom-on-input 'all)
  )

(add-hook 'eshell-first-time-mode-hook #'gatsby:eshell--setup)

(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

(setq eshell-hist-ignoredups t)

(setq eshell-history-size 10000)

(require 'esh-mode)
(add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions
      (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

(setq eshell-buffer-maximum-lines 12000)
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

(setq eshell-list-files-after-cd t)

(setq eshell-error-if-no-glob t
      eshell-glob-case-insensitive t)

(setq eshell-destroy-buffer-when-process-dies t)

(setq eshell-scroll-to-bottom-on-input 'all)

(use-package em-term :straight (em-term :type built-in) :after eshell)

(defun gatsby:eshell-history ()
  "Search history"
  (interactive)
  (let* ((selectrum-should-sort nil)
         (eshell-bol (save-excursion (eshell-bol) (point)))
         (command (completing-read
                   "History: "
                   (if eshell-history-ring
                       (-distinct (ring-elements eshell-history-ring))
                     '())
                   nil nil
                   (buffer-substring eshell-bol (point)))))
    (delete-region eshell-bol (point))
    (insert command)))

(defun gatsby:eshell-goto-last-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(defun gatsby:eshell--change-buffer-title ()
  "Change the title of eshell buffer to reflect $pwd."
  (rename-buffer (format "%s: %s" eshell-buffer-name (directory-file-name default-directory)) 'unique))

(add-hook 'eshell-mode-hook #'gatsby:eshell--change-buffer-title)
(add-hook 'eshell-directory-change-hook #'gatsby:eshell--change-buffer-title)

(defun gatsby:eshell-clear-buffer ()
  "Eshell version of `cls'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun gatsby:eshell-toggle-sudo ()
  "Add/Remove sudo in the begining of command line."
  (interactive)
  (let ((pos (point))
        (commands (buffer-substring-no-properties
                   (eshell-bol) (point-max))))
    (if (string-match-p "^sudo " commands)
        (progn
          (eshell-bol)
          (while (re-search-forward "sudo " nil t)
            (replace-match "" t nil))
          (setq pos (- pos 4)))
      (eshell-bol)
      (insert "sudo ")
      (setq pos (+ pos 6)))
    (goto-char pos)
    (evil-insert-state)))

(evil-define-operator eshell/evil-change (beg end type register yank-handler delete-func)
  "Like `evil-change' but will not delete/copy the prompt."
  (interactive "<R><x><y>")
  (save-restriction
    (narrow-to-region eshell-last-output-end (point-max))
    (evil-change (max beg (point-min))
                 (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                 type register yank-handler delete-func)))

(evil-define-operator eshell/evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (eshell/evil-change beg end type register yank-handler #'evil-delete-line))

(evil-define-operator eshell/evil-delete (beg end type register yank-handler)
  "Like `evil-delete' but will not delete/copy the prompt."
  (interactive "<R><x><y>")
  (save-restriction
    (narrow-to-region eshell-last-output-end (point-max))
    (evil-delete (if beg (max beg (point-min)) (point-min))
                 (if (eq type 'line) (point-max) (min (or end (point-max)) (point-max)))
                 type register yank-handler)))

(evil-define-operator eshell/evil-delete-line (_beg end type register yank-handler)
  "Change to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (eshell/evil-delete (point) end type register yank-handler))

(defun gatsby:eshell--setkey ()
  "Customize key in eshell-mode."
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'eshell-mode-map
   "A" 'gatsby:eshell-goto-last-prompt
   "H" 'eshell-bol
   "S" 'gatsby:eshell-toggle-sudo
   "c" 'eshell/evil-change
   "C" 'eshell/evil-change-line
   "d" 'eshell/evil-delete
   "D" 'eshell/evil-delete-line
   "<" 'eshell-previous-prompt
   ">" 'eshell-next-prompt)

  (general-define-key
   :states 'insert
   :keymaps 'eshell-mode-map
   "C-r" 'gatsby:eshell-history)

  (general-define-key
   :states '(normal visual motion emacs insert)
   :keymaps 'eshell-mode-map
   :prefix "C-c"
   "C-l" 'gatsby:eshell-clear-buffer)

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'eshell-mode-map
   :prefix "SPC"
   "q" 'kill-buffer-and-window))

(add-hook 'eshell-first-time-mode-hook #'gatsby:eshell--setkey)

(defun eshell/x (file &rest args)
  "Unpack FILE with ARGS using default command."
  (let* ((command
          (-some (lambda (x)
                   (if (string-match-p (car x) file)
                       (cadr x)))
                 '((".*\.tar.bz2" "tar xjf")
                   (".*\.tar.gz" "tar xzf")
                   (".*\.bz2" "bunzip2")
                   (".*\.rar" "unrar x")
                   (".*\.gz" "gunzip")
                   (".*\.tar" "tar xf")
                   (".*\.tbz2" "tar xjf")
                   (".*\.tgz" "tar xzf")
                   (".*\.zip" "unzip")
                   (".*\.Z" "uncompress")
                   (".*" "echo 'Could not unpack the file:'"))))
         (unpack-command
          (concat command " " file " " (mapconcat 'identity args " "))))
    (eshell/printnl "Unpack command: " unpack-command)
    (eshell-command-result unpack-command)))

(defun eshell/mkcd (dir &rest _)
  "Run \"mkdir dir\" then \"cd dir\""
  (interactive)
  (eshell/mkdir dir)
  (eshell/cd dir))

(defun eshell/ff (&rest files)
  "Open FILES in emacs."
  (setq files (flatten-tree files))
  (mapc 'find-file files))

(defun eshell/FF (&rest files)
  "Open FILES in a new window in emacs."
  (setq files (flatten-tree files))
  (gatsby:core-split-window)
  (other-window 1)
  (mapc 'find-file files))

(defun gatsby:eshell-open-here (&optional arg)
  "Open a new shell in the current directory.
If the prefix argument (ARG) is not null, go to the home directory.
If there is already a eshell buffer open for that directory, switch to that buffer."
  (interactive "P")
  (let* ((dir (if arg (expand-file-name "~/") default-directory))
         ;; check whether there exists a eshell buffer for DIR
         (exists (--first (with-current-buffer it
                            (and (string-equal major-mode "eshell-mode")
                                 (f-equal-p dir default-directory)))
                          (buffer-list)))
         ;; check if the matched eshell buffer is visible
         (visible (when exists (get-buffer-window exists 'all-frames))))
    (if visible
        (select-window visible)
      (split-window-below (- (/ (window-total-height) 3)))
      (other-window 1)
      (if exists
          (switch-to-buffer exists)
        (let ((default-directory dir))
          (eshell 'Z))))
    (goto-char (point-max))
    (evil-insert-state)))

(general-define-key :keymaps '(motion normal visual) :prefix "SPC"
  "os" 'gatsby:eshell-open-here)

(use-package tramp :straight (:type built-in))
(use-package em-tramp :straight (:type built-in))

(setq tramp-histfile-override "/dev/null")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun gatsby:eshell-sudo (&rest commands)
  "Use `tramp' run COMMAND in /sudo::`default-directory'.  Does not have any flags so won't get error if -i or --user is given."
  (setq commands (flatten-tree commands))
  (if (not commands)
      (gatsby:eshell-su)
    (throw 'eshell-external
           (let ((user "root")
                 (host (or (file-remote-p default-directory 'host) "localhost"))
                 (dir (file-local-name (expand-file-name default-directory)))
                 (prefix (file-remote-p default-directory))
                 (sudo? (string-equal "sudo" (file-remote-p default-directory 'method))))
             (cond (sudo?
                    (eshell-named-command (car commands) (cdr commands)))
                   (prefix
                    (let ((default-directory (format "%s|sudo:%s@%s:%s"
                                                     (substring prefix 0 -1) user host dir)))
                      (eshell-named-command (car commands) (cdr commands))))
                   (t
                    (let ((default-directory (format "/sudo:%s@%s:%s" user host dir)))
                      (eshell-named-command (car commands) (cdr commands)))))))))

(advice-add #'eshell/sudo :override #'gatsby:eshell-sudo)

;; override eshell/su
(defun gatsby:eshell-su (&rest _)
  "toggle between `default-directory' and /sudo::`default-directory'."
  (let ((user "root")
        (host (or (file-remote-p default-directory 'host) "localhost"))
        (dir (file-local-name (expand-file-name default-directory)))
        (prefix (file-remote-p default-directory))
        (sudo? (string-equal "sudo" (file-remote-p default-directory 'method))))
    (if sudo?
        ;; in sudo mode, go back to non-sudo
        (let ((new-prefix (replace-regexp-in-string
                           (format "[|/]sudo:root@%s" host) ""
                           prefix)))
          (eshell/cd (if (string= ":" new-prefix) dir (format "%s%s" new-prefix dir))))
      ;; in non-sudo mode, go to sudo
      (if prefix
          (eshell/cd
           (format "%s|sudo:%s@%s:%s" (substring prefix 0 -1) user host dir))
        (eshell/cd (format "/sudo:%s@%s:%s" user host dir))))))

(advice-add #'eshell/su :override #'gatsby:eshell-su)

(defun gatsby:eshell-cd (cd &rest args)
  "Make `eshell/cd' tramp-aware."
  (let* ((host (file-remote-p default-directory))
         (home (format "%s/home/%s" host (file-remote-p default-directory 'user))))
    (if (and (not args)
             host)
        (if (file-exists-p home)
            (funcall cd home)
          (funcall cd (format "%s/" host)))
      (funcall cd args))))

(advice-add #'eshell/cd :around #'gatsby:eshell-cd)

(with-eval-after-load 'envrc
  (defun gatsby:envrc--update-after-cd (&rest _)
    "Update the current direnv environment after `eshell/cd'."
    (let ((buf (current-buffer)))
      (if (f-traverse-upwards (lambda (dir)
                                (f-exists-p (format "%s/.envrc" dir))))
          (envrc--update)
        (envrc--clear buf)
        (setq envrc--status 'none))))

  (advice-add #'eshell/cd :after #'gatsby:envrc--update-after-cd))

(use-package org
  :straight (org :host github :repo "yantar92/org" :branch "feature/org-fold"
                 :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")))

(defun gatsby:org--enable-extra-pairs ()
  (make-variable-buffer-local 'electric-pair-pairs)
  (add-to-list 'electric-pair-pairs '(?\~ . ?\~)))

(add-hook 'org-mode-hook #'gatsby:org--enable-extra-pairs)

(defun gatsby:org--fix-indent () (setq tab-width 2))
(add-hook 'org-mode-hook #'gatsby:org--fix-indent)

(setq org-startup-indented t
      org-startup-with-latex-preview t
      org-preview-latex-image-directory ".org-latex-imgcache/")

(setq org-use-sub-superscripts nil)

(setq org-hide-leading-stars nil
      org-indent-mode-turns-on-hiding-stars nil)

(defun gatsby:org--complete-keywords ()
  "Allow company to complete org keywords after ^#+"
  (add-hook 'completion-at-point-functions
            'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook #'company-mode)
(add-hook 'org-mode-hook #'gatsby:org--complete-keywords)

(defcustom gatsby:org-foldable'(example-block export-block src-block table)
  "A list of org-element that are consider foldable, and hence can be folded/expanded by `gatsby:org-hide-block' and `gatsby:org-show-block'."
  :type '(list symbol)
  :group 'org)

(defun gatsby:org-hide-block ()
  "Hide current block, if it is inside element defined in `gatsby:org-foldable', first try to fold the element.  Fall back to `evil-close-fold'."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (memq (car element) gatsby:org-foldable)
        (progn
          (goto-char (plist-get (cadr element) :begin))
          (org-hide-block-toggle t))
      (evil-close-fold))))

(defun gatsby:org-show-block ()
  "Show current block."
  (interactive)
  (condition-case _
      (org-hide-block-toggle 'off)
    (error (evil-open-fold))))

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion)
  "zo" #'gatsby:org-show-block
  "zc" #'gatsby:org-hide-block)

(setq org-cycle-emulate-tab nil)

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion) :prefix "SPC"
  "rf" #'org-footnote)

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion)
  "<" #'org-previous-visible-heading
  ">" #'org-next-visible-heading)

(general-define-key :keymaps 'org-mode-map :states 'normal
  "RET" #'org-open-at-point)

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear" :branch "feature/org-fold-support")
  :hook (org-mode . org-appear-mode))

(setq org-appear-autolinks t)

(setq org-export-backends '(ascii html latex md odt)
      org-export-coding-system 'utf-8
      org-export-with-sub-superscripts t
      org-export-with-toc nil
      org-latex-packages-alist '(("" "setspace")
                                 ;; https://github.com/gpoore/minted/issues/92
                                 ("cache=false" "minted")
                                 ("" "pdflscape")
                                 ("" "multirow")
                                 ("" "multicol")
                                 ("" "booktabs")
                                 ("" "amsthm")
                                 ("" "amssymb")
                                 ("" "listingsutf8")
                                 ("top=1in, bottom=1in, left=1in, right=1in" "geometry")
                                 ("" "natbib"))
      org-latex-listings 'minted
      org-highlight-latex-and-related '(latex entities script)
      org-latex-pdf-process
      '("pdflatex -shell-escape -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion) :prefix "SPC"
  "re" #'org-export-dispatch)

(setq org-src-window-setup 'plain)

(setq org-confirm-babel-evaluate nil)

(setq org-src-ask-before-returning-to-edit-buffer nil)

(defun gatsby:org--ensure-normal-state (&rest _)
  (evil-normal-state))

(advice-add #'org-edit-src-code :before #'gatsby:org--ensure-normal-state)

(setq org-babel-load-languages '((emacs-lisp . t)
                                 (shell . t)))

(defconst gatsby:org-babel-default-interpreters '("emacs-lisp" "shell")
  "List of interpreters that should be available at all environments.")

(defvar gatsby:org-babel-available-interpreters '("emacs-lisp" "shell")
  "List of interpreters available in the current environment.")

(make-variable-buffer-local 'gatsby:org-babel-available-interpreters)

(defvar gatsby:org-babel-hook nil
  "Hook run with `org-mode-hook', but specific to my babel setting.")

(defun gatsby:org-babel-run-hook ()
  (run-hooks 'gatsby:org-babel-hook))

(add-hook 'org-mode-hook #'gatsby:org-babel-run-hook)

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion) :prefix "SPC"
  "rr" #'org-ctrl-c-ctrl-c)

(add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

(defun gatsby:org-remove-all-results ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))

(general-define-key :keymaps 'org-mode-map :states '(normal visual motion) :prefix "SPC"
  "rc" #'gatsby:org-remove-all-results)

(use-package ob-async)

(use-package flymake :hook (prog-mode . flymake-mode))

(setq flymake-start-on-newline nil)

(defun gatsby:flymake-enter-normal-state-check ()
  "Check for syntax errors when entering normal state"
  (when (bound-and-true-p evil-mode)
    (flymake-log :debug "starting syntax check as entering normal state")
    (flymake-start t)))

(add-hook 'evil-normal-state-entry-hook #'gatsby:flymake-enter-normal-state-check)

(define-fringe-bitmap 'gatsby:double-error-bitmap
  [#b11011000
   #b01101100
   #b00110110
   #b00011011
   #b00110110
   #b01101100
   #b11011000])

(setq flymake-note-bitmap '(gatsby:double-error-bitmap default)
      flymake-warning-bitmap '(gatsby:double-error-bitmap compilation-warning)
      flymake-error-bitmap '(gatsby:double-error-bitmap compilation-error))

(dolist (face '(flymake-note flymake-warning flymake-error))
  (set-face-attribute face nil :underline nil))

(advice-add #'flymake-eldoc-function :override #'ignore)

(use-package flymake-childframe
  :straight (flymake-childframe :repo "junyi-hou/flymake-childframe" :host github)
  :hook (flymake-mode . flymake-childframe-mode))

(setq flymake-childframe-prefix '((note . "[INFO]")
                                  (warning . "[WARN]")
                                  (error . "[ERR]")))

(ligature-set-ligatures 'flymake-childframe-buffer-mode
                        '("[INFO]" "[WARN]" "[ERR]"))

(use-package company
  :straight (company :files (:defaults "icons")))
(add-hook 'prog-mode-hook #'company-mode)

(setq company-idle-delay nil
      company-require-match 'never
      company-search-regexp-function #'company-search-words-in-any-order-regexp)

(defun gatsby:indent-or-complete ()
  "Tab for company, yas, and indentation.
  1, if nothing before the `point', insert `tab-width' number of spaces.
  2, if there is a yas-snippet, expand it.
  3, if 1/2 not satisfies, call `company-manual-begin'."
  (interactive)
  (if (looking-back "\\([[:space:]]+\\|^\\=\\)" (line-beginning-position)) ;; case 1
      (insert-tab)
    (unless (yas-expand) ;; case 2
      ;; case 3
      (unless (symbol-value company-search-mode)
        (company-filter-candidates)))))

(general-define-key :keymaps 'insert
  "<tab>" 'gatsby:indent-or-complete)

(defun gatsby:company-filter-delete ()
  "improve `company-search-delete-char'."
  (if (string= company-search-string "")
      (progn
        (company-search-abort)
        (call-interactively #'backward-delete-char-untabify)
        (company-filter-candidates))
    (let ((ss (substring company-search-string 0 -1)))
      (when company-search-filtering
        (company--search-update-predicate ss))
      (company--search-update-string ss))))

(advice-add #'company-search-delete-char :override #'gatsby:company-filter-delete)

(defun gatsby:company-filter-abort ()
  "Abort both the `company-search-mode' and the `company-mode'"
  (interactive)
  (ignore-errors (company-search-abort))
  (company-abort))

(general-define-key :keymaps 'company-active-map
  "M-j" 'company-select-next
  "M-k" 'company-select-previous
  "<tab>" 'company-complete-selection)

(general-define-key :keymaps 'company-search-map
  "M-j" 'company-select-next
  "M-k" 'company-select-previous
  "C-g" 'gatsby:company-filter-abort
  "<tab>" 'company-complete-selection)

(setq company-auto-commit-chars '(?\( ?. 44))

(defun gatsby:company-complete-and-insert-char (char)
  "Complete with the current selection and insert CHAR."
  (company-complete-selection)
  (insert (char-to-string char)))

;; bind auto complete chars in `company-search-map'
(--each company-auto-commit-chars
  (define-key company-search-map (char-to-string it)
    (lambda () (interactive) (gatsby:company-complete-and-insert-char it))))

(with-eval-after-load 'flymake
  (setq flymake-no-changes-timeout nil))

(setq company-show-numbers t
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-selection-wrap-around t
      company-auto-commit t)

(setq company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-code-other-buffers nil
      company-backends '((:separate company-capf company-yasnippet)
                         company-files
                         (company-dabbrev-code company-dabbrev)))

(use-package company-prescient :hook (company-mode . company-prescient-mode))

(use-package yasnippet
  :hook
  (company-mode . yas-reload-all)
  (company-mode . yas-minor-mode)
  :config
  (setq yas-triggers-in-field t)
  (defun gatsby:yas--clear-field-filter (cmd)
    "Clear field regardless of whether it has been modified or not.
  This make sure that if I backtab into a modified field I can still hit <Del> and delete the whole field."
    (let ((field (yas-current-field)))
      (when (and field
                 (eq (point) (marker-position (yas--field-start field))))
        cmd)))
  
  (advice-add #'yas--maybe-clear-field-filter :override #'gatsby:yas--clear-field-filter)
  
  (defun gatsby:yas-better-backspace ()
    "If `point' is at the beginning of an unmodified yas-field, delete the field, otherwise backwards delete char."
    (interactive)
    (cond ((yas--maybe-clear-field-filter t)
           (yas--skip-and-clear (yas-current-field)))
          (t (call-interactively #'backward-delete-char-untabify))))
  
  (general-define-key :keymaps 'yas-keymap "<backspace>" 'gatsby:yas-better-backspace)
  (add-to-list 'yas-prompt-functions #'completing-read))

(use-package eglot
  :custom-face
  ;; less invasive symbol highlight
  (eglot-highlight-symbol-face ((t :underline t))))

(setq eglot-stay-out-of '(company))

(use-package help-childframe
  :straight
  (help-childframe :host github :repo "junyi-hou/help-childframe.el")
  :general
  (:keymaps '(normal visual motion)
   :prefix "SPC"
   "rh" #'help-childframe-show))

(add-hook 'after-init-hook #'global-help-childframe-mode)

(use-package tree-sitter
  :straight
  (:files (:defaults "langs/*.el" "langs/bin" "langs/queries"))
  :hook
  (tree-sitter-mode . tree-sitter-hl-mode)
  (tree-sitter-mode . tree-sitter-fold-mode))

(use-package tree-sitter-langs
  :straight (:type built-in))

;; overriding `:files' attribute breaks hard-coded variable
;; `tree-sitter-langs-git-dir', which points to
;; `straight/repos/tree-sitter-langs/'. Move it to the correct location
;; `straight/repos/emacs-tree-sitter/langs/' instead.
(setq tree-sitter-langs-git-dir (straight--repos-dir "emacs-tree-sitter/langs"))

(use-package tree-sitter-fold
  :straight (:host github :repo "junyi-hou/tree-sitter-fold"))

(use-package comint  :straight (:type built-in))

(defun gatsby:comint-goto-last-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(defun gatsby:comint-cls ()
  "clear current REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun gatsby:comint-return ()
  "<return> sends a newline character, and double <return> sends `comint-send-input'."
  (interactive)
  (let* ((text-before-point (buffer-substring (save-excursion (comint-bol)) (point)))
         (open-b (s-count-matches "\\[" text-before-point))
         (close-b (s-count-matches "\\]" text-before-point))
         (open-c (s-count-matches "{" text-before-point))
         (close-c (s-count-matches "}" text-before-point))
         (open-p (s-count-matches "(" text-before-point))
         (close-p (s-count-matches ")" text-before-point)))
    (if (or (eq last-command this-command)
            (and (= open-b close-b)
                 (= open-c close-c)
                 (= open-p close-p)))
        (call-interactively 'comint-send-input)
      (call-interactively 'newline))))

(defcustom gatsby:comint-repl-mode-alist nil
  "An alist of (code-buffer-mode . repl-mode)."
  :type 'alist
  :group 'comint)

(defvar-local gatsby:comint-repl-buffer nil
  "The repl associated to the current buffer.")

(defun gatsby:comint-associate-repl ()
  "Select a repl of mode REPL-MODE and associate the current buffer to that repl."
  (interactive)
  (let* ((repl-mode (cdr (assq major-mode gatsby:comint-repl-mode-alist)))
         (candidates (-map 'buffer-name
                           (--filter
                            (with-current-buffer it
                              (eq major-mode repl-mode))
                            (buffer-list))))
         (repl-name (completing-read "Choose REPL to associate to: "
                                     (or candidates (buffer-list)))))

    (setq gatsby:comint-repl-buffer (get-buffer repl-name))))

(general-define-key :keymaps 'comint-mode-map :states '(normal visual motion emacs insert) :prefix "C-c"
  "C-l" 'gatsby:comint-cls
  "C-c" 'comint-interrupt-subjob)

(general-define-key :keymaps 'comint-mode-map :states 'insert
  "<up>" 'comint-previous-matching-input-from-input
  "<down>" 'comint-next-matching-input-from-input
  "<return>" 'gatsby:comint-return)

(general-define-key :keymaps 'comint-mode-map :states '(normal motion visual insert emacs)
  "C-j" 'evil-window-down
  "C-d" 'evil-scroll-down)

(general-define-key :keymaps 'comint-mode-map :states '(normal motion visual)
  "<" 'comint-previous-prompt
  ">" 'comint-next-prompt
  "A" 'gatsby:comint-goto-last-prompt
  "H" 'comint-bol
  "SPC" 'nil)

(defcustom gatsby:comint-repl-function-alist nil
  "An alist of (code-buffer-mode . (function to call REPL . argument if any))."
  :type 'alist
  :group 'comint)

(defun gatsby:comint--start-repl (repl-fn &rest args)
  "Start a REPL in the background using REPL-FN with ARGS."
  (let ((code-buffer (current-buffer))
        (display-buffer-alist `(,(cons "*" (cons #'display-buffer-no-window nil)))))
    (let ((repl-buffer (apply repl-fn args)))
      (with-current-buffer code-buffer
        (setq-local gatsby:comint-repl-buffer repl-buffer)))))

(defun gatsby:comint--pop-to-repl ()
  "Switch to `gatsby:comint-repl-buffer' associated with the current buffer."
  (if (and gatsby:comint-repl-buffer
           (buffer-live-p gatsby:comint-repl-buffer))
      (progn
        (pop-to-buffer gatsby:comint-repl-buffer)
        (select-window (get-buffer-window gatsby:comint-repl-buffer)))
    (user-error "Buffer not associated with a REPL")))

(defun gatsby:comint-start-or-pop-to-repl ()
  "Start (or pop to the existing) repl buffer for the current buffer."
  (interactive)
  (unless (and gatsby:comint-repl-buffer
               (buffer-live-p gatsby:comint-repl-buffer))
    (apply #'gatsby:comint--start-repl
           (cdr (assq major-mode gatsby:comint-repl-function-alist))))
  (gatsby:comint--pop-to-repl))

(defun gatsby:comint--send-code-to-repl (send-fn string)
  "Send STRING using SEND-FN to the associated buffer for evaluation."
  (if gatsby:comint-repl-buffer
      (with-current-buffer gatsby:comint-repl-buffer
        (goto-char (point-max))
        (let ((bol (save-excursion
                     (comint-bol))))
          (delete-region bol (point-max)))
        (insert string)
        (funcall send-fn))
    (user-error "Associated REPL not found")))

(defun gatsby:comint--eval-region (send-fn beg end)
  (gatsby:comint--send-code-to-repl send-fn (buffer-substring-no-properties beg end))
  (deactivate-mark))

(defun gatsby:comint-eval-region-or-line ()
  (interactive)
  (if (region-active-p)
      (gatsby:comint--eval-region 'comint-send-input
                                  (region-beginning) (region-end))
    (gatsby:comint--send-code-to-repl 'comint-send-input
                                      (thing-at-point 'line 'no-properties))))

(defun gatsby:comint-eval-buffer ()
  (interactive)
  (gatsby:comint--send-code-to-repl
   'comint-send-input
   (buffer-substring-no-properties (point-min) (point-max))))

(defun gatsby:comint-exit-repl ()
  "When quitting repl buffer, reset `gatsby:comint-repl-buffer' for all associated code buffers."
  (interactive)
  (when (y-or-n-p "Really quit this REPL? ")
    (let ((repl-buffer (current-buffer)))
      (--each (--filter (with-current-buffer it (eq gatsby:comint-repl-buffer repl-buffer))
                        (buffer-list))
        (with-current-buffer it
          (setq-local gatsby:comint-repl-buffer nil))))
    (kill-buffer-and-window)))

(general-define-key :keymaps 'comint-mode-map :states '(normal motion visual) :prefix "SPC"
  "q" 'gatsby:comint-exit-repl)

(use-package jupyter

:straight
(:no-native-compile t :host github :repo "nnicandro/emacs-jupyter")

:custom-face
(jupyter-repl-traceback ((t (:extend t :background "firebrick")))))

(defun gatsby:jupyter-repl-cell-code ()
  "Return the code of the current cell, tweaked to stripped text properties to make sure things like `highlight-indent-guides' does not get picked up."
  (buffer-substring-no-properties
   (jupyter-repl-cell-code-beginning-position)
   (jupyter-repl-cell-code-end-position)))

(advice-add #'jupyter-repl-cell-code :override #'gatsby:jupyter-repl-cell-code)

(defun gatsby:jupyter--encode-before-eval (fn &rest args)
  "advice `jupyter-eval-string' to encode first the strings with utf-8."
  (let (str)
    (if (cdr args)
        (setq str (apply #'buffer-substring-no-properties (cdr args)))
      (setq str (car args)))
    (funcall fn (encode-coding-string str 'utf-8))))

(advice-add #'jupyter-eval-string :around #'gatsby:jupyter--encode-before-eval)

(defun jupyter-repl-font-lock-override (_ignore beg end &optional verbose)
  `(jit-lock-bounds ,beg . ,end))

(advice-add #'jupyter-repl-font-lock-fontify-region :override #'jupyter-repl-font-lock-override)

(advice-add #'jupyter-repl-isearch-setup :override #'ignore)

(advice-add #'jupyter-command :around #'envrc-propagate-environment)

(defun gatsby:jupyter--update-kernelspecs (&rest _)
  "Update kernelspecs before running any REPL."
  (jupyter-available-kernelspecs 'refresh))

(advice-add #'jupyter-run-repl :before #'gatsby:jupyter--update-kernelspecs)

(setq jupyter-repl-echo-eval-p t)

(setq jupyter-repl-maximum-size 12000)

(setq jupyter-repl-history-maximum-length 5000)

(defun gatsby:jupyter--pop-repl (&rest _)
  "Pop repl buffer, then go back to the code buffer."
  (let* ((code-buffer (current-buffer)))
    (jupyter-repl-pop-to-buffer)
    (switch-to-buffer-other-window code-buffer)))

(advice-add #'jupyter-eval-line-or-region :before #'gatsby:jupyter--pop-repl)

(defun gatsby:jupyter--deactivate-mark (&rest _)
  "Deactivate mark, use &rest to satisfies the number of arguments"
  (deactivate-mark))

(advice-add #'jupyter-eval-region :after #'gatsby:jupyter--deactivate-mark)

(defun gatsby:jupyter-goto-last-prompt ()
  "Goto current prompt and continue editting."
  (interactive)
  (goto-char (point-max))
  (evil-insert 1))

(general-define-key :keymaps 'jupyter-repl-mode-map :states '(normal visual motion)
  "A" #'gatsby:jupyter-goto-last-prompt
  "<" #'jupyter-repl-backward-cell
  ">" #'jupyter-repl-forward-cell
  "SPC" nil)

(general-define-key :keymaps 'jupyter-repl-mode-map :states '(normal visual motion) :prefix "C-c"
  "C-c" #'jupyter-repl-interrupt-kernel
  "C-l" #'jupyter-repl-clear-cells)

(general-define-key :keymaps 'jupyter-repl-mode-map :states '(normal visual motion) :prefix "SPC"
  "q" #'kill-buffer-and-window)

(general-define-key :keymaps 'jupyter-repl-mode-map :states 'insert
  "<up>" #'jupyter-repl-history-previous-matching
  "<down>" #'jupyter-repl-history-next-matching)

(defcustom gatsby:jupyter-repl-function-alist nil
  "An alist of (code-buffer-mode . action), where action can be:
- a string indicating the kernel name should be called in CODE-BUFFER-MODE
- a function with no argument that should be called to start the corresponding jupyter REPL in CODE-BUFFER-MODE"
  :type 'alist
  :group 'jupyter)

(defun gatsby:jupyter-start-or-switch-to-repl ()
  "Switch to REPL associated the current buffer.  If there is no REPL associated with the current buffer, start one according to KERNEL type."
  (interactive)
  (if (and jupyter-current-client
           (jupyter-kernel-alive-p jupyter-current-client))
      (jupyter-repl-pop-to-buffer)
    (let ((code-buffer (current-buffer))
          (major-mode-action (cdr (assq major-mode gatsby:jupyter-repl-function-alist))))
      (cond ((stringp major-mode-action)
             (jupyter-run-repl major-mode-action major-mode-action (current-buffer) nil t))
            ((functionp major-mode-action)
             (funcall major-mode-action))
            (t (call-interactively 'jupyter-run-repl)))
      (switch-to-buffer-other-window code-buffer))))

(add-hook 'jupyter-repl-mode-hook #'company-mode)

(add-to-list 'ligature-composition-table
             `(jupyter-repl-mode ,@(cdr (assq 'prog-mode ligature-composition-table))))

(defun gatsby:jupyter--register-local-kernels (kernel-names)
  ;; fix stata kernel issues
  (when (and (member "stata" kernel-names)
             (not (boundp 'inferior-STA-program-name)))
    (setq inferior-STA-program-name "stata-mp"))

  ;; load languages
  (let* ((kernel-names '("python3" "stata"))
         (jupyter '(jupyter . t))
         (kernels `(,@(--map `(,(intern it) . t) kernel-names)))
         (interpreters (if (assq 'jupyter org-babel-load-languages)
                           `(,@(butlast org-babel-load-languages)
                             ,@kernels
                             ,jupyter)
                         `(,@org-babel-load-languages
                           ,@kernels
                           ,jupyter))))
    (org-babel-do-load-languages 'org-babel-load-languages interpreters)))

(defun gatsby:jupyter--update-available-interpreters (kernel-names)
  (setq-local gatsby:org-babel-available-interpreters
              `(,@gatsby:org-babel-default-interpreters
                ,@(--map (format "jupyter-%s" it) kernel-names))))

(make-variable-buffer-local 'ob-async-no-async-languages-alist)

(defun gatsby:jupyter--enable-async-block (kernel-names)
  ;; let jupyter manager its own async state
  (setq-local ob-async-no-async-languages-alist
              `(,@(--map (format "jupyter-%s" it) kernel-names)))

  ;; set jupyter languages default header argument
  (--each kernel-names
    (eval `(setq ,(intern (format "org-babel-default-header-args:jupyter-%s" it))
                 (quote ((:async . "yes")
                         (:session . ,it)
                         (:kernel . ,it)))))))

(defun gatsby:jupyter--pre-are-local-env-for-org ()
  (when (executable-find "jupyter")
    (let* ((_kernels (jupyter-available-kernelspecs 'refresh))
           (kernel-names (--map (plist-get (cddr it) :language) _kernels)))
      (gatsby:jupyter--register-local-kernels kernel-names)
      (gatsby:jupyter--update-available-interpreters kernel-names)
      (gatsby:jupyter--enable-async-block kernel-names))))

(add-hook 'gatsby:org-babel-hook #'gatsby:jupyter--pre-are-local-env-for-org)

(use-package ein)

(setq ein:output-area-inlined-images t)

(use-package flyspell-correct)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(defun gatsby:style-flyspell-correct-selectrum (candidates word)
  "Run `completing-read' for the given CANDIDATES

List of CANDIDATES is given by flyspell for the WORD."
  (let* ((selectrum-should-sort nil)
         (save "[SAVE]")
         (accept-session "[ACCEPT (session)]")
         (accept-buffer "[ACCEPT (buffer)]")
         (skip "[SKIP]")
         (result (completing-read
                  (format "Correcting '%s': " word)
                  (append (list save accept-session accept-buffer skip)
                          candidates)
                  nil nil nil nil (car candidates))))
    (cond
     ((string= result save)
      (cons 'save word))
     ((string= result accept-session)
      (cons 'session word))
     ((string= result accept-buffer)
      (cons 'buffer word))
     ((string= result skip)
      (cons 'skip word))
     (t
      result))))

(setq flyspell-correct-interface #'gatsby:style-flyspell-correct-selectrum)

(setq langtool-bin "languagetool-commandline")
(use-package langtool)

(defun gatsby:langtool--display-message (overlays)
  (let ((msg (langtool-details-error-message overlays)))
    (eldoc-box--eldoc-message-function msg)))

(setq langtool-autoshow-message-function #'gatsby:langtool--display-message)

(defvar gatsby:langtool--in-process nil
  "Non-nil means we are in the process of checking grammar.")

(defun gatsby:langtool-toggle (&optional lang)
  "Toggle languagetool check."
  (interactive (when current-prefix-arg
                 (list (langtool-read-lang-name))))
  (if gatsby:langtool--in-process
      (progn
        (langtool-check-done)
        (setq gatsby:langtool--in-process nil))
    (langtool-check-buffer lang)
    (setq gatsby:langtool--in-process t)))

(add-hook 'langtool-noerror-hook (lambda (&rest _) (setq gatsby:langtool--in-process nil)))

(general-define-key :keymaps '(motion normal visual) :prefix "SPC"
  "cc" 'flyspell-correct-wrapper
  "cg" 'gatsby:langtool-toggle)

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'gatsby:accounts)

(use-package epa

  :custom
  (epg-pinentry-mode 'loopback)
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  (epa-file-encrypt-to user-mail-address)
  (password-cache t)
  (password-cache-expiry 720000))

(use-package password-store)

(setq password-store-password-length 16)

(defun gatsby:password-store-generate (entry &optional password-length)
  "Generate a new password for ENTRY with PASSWORD-LENGTH, checking for duplicates first."
  (interactive (list (read-string "Password entry: ")))
  (let ((password-length (or password-length password-store-password-length)))
    ;; (message (concat "~/.password-store/" entry ".gpg"))
    (when (file-exists-p (concat "~/.password-store/" entry ".gpg"))
      (unless (y-or-n-p (concat entry " already exists, override?"))
        (user-error "Aborting")))
    (password-store--run-generate entry password-length t)))

(advice-add #'password-store--run-generate
            :after (lambda (entry &rest args) (password-store-copy entry)))

(general-define-key :keymaps '(normal visual motion) :prefix "SPC"
  "pc" 'password-store-copy
  "pg" 'gatsby:password-store-generate)

(use-package sudo-edit)

(add-hook 'after-init-hook #'sudo-edit-indicator-mode)

(use-package magit)

(add-to-list 'evil-motion-state-modes 'magit-status-mode)
(add-to-list 'evil-motion-state-modes 'magit-diff-mode)
(add-to-list 'evil-motion-state-modes 'magit-log-mode)
(add-to-list 'evil-motion-state-modes 'magit-revision-mode)
(add-to-list 'evil-motion-state-modes 'magit-process-mode)

(evil-set-initial-state 'git-commit-mode 'insert)

(defcustom magit-push-protected-branch nil
  "When set, ask for confirmation before pushing to this branch (e.g. master).  Set this in .dir-locals.el"
  :type 'list
  :safe 'listp
  :group 'magit)

(defun magit-push--protected-branch (magit-push-fun &rest args)
  "Ask for confirmation before pushing a protected branch."
  (if (member (magit-get-current-branch) magit-push-protected-branch)
      ;; Arglist is (BRANCH TARGET ARGS)
      (if (yes-or-no-p (format "Push to protected branch %s? " (magit-get-current-branch)))
          (apply magit-push-fun args)
        (error "Push aborted by user"))
    (apply magit-push-fun args)))

(advice-add 'magit-push-current-to-pushremote :around #'magit-push--protected-branch)
(advice-add 'magit-push-current-to-upstream :around #'magit-push--protected-branch)

(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        'magit-insert-unpushed-to-upstream-or-recent
                        'replace)

(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-unpushed-to-upstream
                        'magit-insert-unpushed-to-pushremote
                        'append)

(defun gatsby:vcs-visit-thing-at-point ()
  "Get file at point in magit buffers."
  (interactive)
  (cond ((magit-section-match '([file] [hunk]))
         (let ((file (magit-file-at-point t)))
           (unless file
             (error "No file at point"))
           (magit-diff-visit-file--internal file nil #'switch-to-buffer-other-window)))
        ((magit-section-match [commit])
         ;; commits: show the commit details
         (call-interactively #'magit-show-commit))
        ((magit-section-match [* error])
         (magit-process-buffer))
        ((magit-section-match [stash])
         (call-interactively #'magit-ediff-show-stash))
        ((and (magit-section-match '(issue pullreq))
              (featurep 'forge))
         ;; for `forge-issue' and `forge-pullreq' block, visit corresponding issue
         (call-interactively #'forge-visit-topic))
        ;; fallback - `magit-visit-thing'
        (t 'magit-visit-thing)))

(general-define-key :keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
                    :states '(motion normal visual)
                    "RET" #'gatsby:vcs-visit-thing-at-point)

(general-define-key :keymaps '(magit-status-mode-map magit-diff-mode-map magit-log-mode-map)
                    :states '(motion normal visual)
                    "SPC" nil
                    ">" 'magit-section-forward-sibling
                    "<" 'magit-section-backward-sibling
                    "zo" 'magit-section-show
                    "zc" 'magit-section-hide
                    "`" 'magit-dispatch)

(general-define-key :keymaps '(magit-status-mode-map
                               magit-diff-mode-map
                               magit-log-mode-map
                               magit-revision-mode-map)
                    :states '(motion normal)
                    :prefix "SPC"
                    "r" (lambda () (interactive) (magit-refresh-buffer)))

(general-define-key :keymaps 'magit-status-mode-map :states '(motion normal)
  "d" 'magit-discard
  "E" 'magit-ediff
  "c" 'magit-commit
  "p" 'magit-push
  "f" 'magit-fetch
  "F" 'magit-pull)

(general-define-key :keymaps '(motion normal visual) :prefix "SPC"
  "gg" #'magit-status
  "gd" #'magit-ediff-show-working-tree
  "gl" #'magit-log-buffer-file
  "gb" #'magit-blame)

(use-package magit-delta)

(add-hook 'magit-mode-hook #'magit-delta-mode)

(setq magit-delta-delta-args `("--max-line-distance" "0.6"
                               "--24-bit-color" "always"
                               "--minus-color=#420000"
                               "--plus-color=#006910"
                               "--plus-emph-color=#00b300"
                               "--color-only"))

(use-package git-rebase :straight (:type built-in))

(add-to-list 'evil-motion-state-modes 'git-rebase-mode)

(general-define-key :keymaps 'git-rebase-mode-map :states 'motion
  "p" 'git-rebase-pick
  "e" 'git-rebase-edit
  "l" 'git-rebase-label
  "r" 'git-rebase-reword
  "s" 'git-rebase-squash
  "d" 'git-rebase-kill-line
  "M-j" 'git-rebase-move-line-down
  "M-k" 'git-rebase-move-line-up)

(use-package forge)

(setq forge-topic-list-limit '(60 . -1))

(general-define-key :keymaps 'magit-status-mode-map
  "@" 'forge-dispatch)

(general-define-key :keymaps 'forge-topic-mode-map :states '(normal visual motion)
  "<return>" 'gatsby:vcs-visit-thing-at-point
  "zo" 'magit-section-show
  "zc" 'magit-section-hide)

(general-define-key :keymaps 'forge-topic-mode-map :states '(normal visual motion) :prefix "SPC"
  "re" 'magit-edit-thing
  "rr" 'forge-create-post)

(use-package ediff)

(add-to-list 'evil-motion-state-modes 'ediff-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun gatsby:vcs-ediff-scroll-left (&optional arg)
  "Scroll left."
  (interactive "P")
  (let ((last-command-event ?>))
    (ediff-scroll-horizontally arg)))

(defun gatsby:vcs-ediff-scroll-right (&optional arg)
  "Scroll right."
  (interactive "P")
  (let ((last-command-event ?<))
    (ediff-scroll-horizontally arg)))

(defun gatsby:vcs-ediff-scroll-up (&optional arg)
  "Scroll up by half of a page."
  (interactive "P")
  (let ((last-command-event ?V))
    (ediff-scroll-vertically arg)))

(defun gatsby:vcs-ediff-scroll-down (&optional arg)
  "Scroll down by half of a page."
  (interactive "P")
  (let ((last-command-event ?v))
    (ediff-scroll-vertically arg)))

(defun gatsby:vcs-ediff-scroll-down-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?v))
    (ediff-scroll-vertically 1)))

(defun gatsby:vcs-ediff-scroll-up-1 ()
  "Scroll down by a line."
  (interactive)
  (let ((last-command-event ?V))
    (ediff-scroll-vertically 1)))

(defun gatsby:vcs-ediff-first-difference ()
  "Jump to first difference."
  (interactive)
  (ediff-jump-to-difference 1))

(defun gatsby:vcs-ediff-last-difference ()
  "Jump to last difference."
  (interactive)
  (ediff-jump-to-difference ediff-number-of-differences))

(defun gatsby:vcs-ediff-modify-keys ()
  "Due to the wired way `ediff-mode' sets up its keymap, need to wrap this in a function and run it in `ediff-keymap-setup-hook'."
  (general-define-key
   :keymaps 'ediff-mode-map
   :states 'motion
   "SPC" nil
   "?" 'ediff-toggle-help
   "n" 'ediff-next-difference
   "N" 'ediff-previous-difference
   "G" 'ediff-jump-to-difference
   "gg" 'gatsby:vcs-ediff-first-difference

   "j" 'gatsby:vcs-ediff-scroll-down-1
   "k" 'gatsby:vcs-ediff-scroll-up-1
   "h" 'gatsby:vcs-ediff-scroll-left
   "l" 'gatsby:vcs-ediff-scroll-right
   "C-d" 'gatsby:vcs-ediff-scroll-down
   "C-u" 'gatsby:vcs-ediff-scroll-up

   "a" 'ediff-copy-A-to-B
   "b" 'ediff-copy-B-to-A

   "C-e" 'ediff-next-difference
   "C-y" 'ediff-previous-difference

   "q" 'ediff-quit)

  ;; fool-proving
  (general-define-key
   :keymaps 'ediff-mode-map
   :states 'motion
   :prefix "SPC"
   "q" 'ediff-quit)

  ;; if it is a three-window job
  (unless ediff-3way-comparison-job
    (general-define-key
     :keymaps 'ediff-mode-map
     :states 'motion
     "a" 'ediff-copy-A-to-C
     "b" 'ediff-copy-B-to-C
     "+" 'ediff-combine-diffs)))

(add-hook 'ediff-keymap-setup-hook #'gatsby:vcs-ediff-modify-keys)

(use-package projectile
  :hook
  (after-init . projectile-mode)
  :init
  (defun gatsby:projectile-find-file (&optional args)
    "Call `projectile-find-file', and pass prefix ARGS to it.
  If no project is found at `default-directory', call `find-file' instead."
    (interactive "P")
    (if (projectile-project-root)
        (projectile--find-file args)
      (call-interactively #'find-file)))
  
  (advice-add #'projectile-find-file :override #'gatsby:projectile-find-file)
  :general
  (:keymaps '(normal motion)
   :prefix "SPC"
   "op" #'projectile-switch-project
   "of" #'projectile-find-file))

(use-package deadgrep)

(defun gatsby:visual-deadgrep (beg end)
  "use `deadgrep' to search visually selected text."
  (interactive "r")
  (deadgrep (buffer-substring-no-properties beg end)))

(defun gatsby:deadgrep-refine-search ()
  (interactive)
  (deadgrep--search-term 'ignore))

(general-define-key :keymaps 'deadgrep-mode-map :states 'emacs
  "j" #'deadgrep-forward
  "J" #'deadgrep-forward-filename
  "k" #'deadgrep-backward
  "K" #'deadgrep-backward-filename
  "r" #'deadgrep-restart
  "s" #'gatsby:deadgrep-refine-search)

(add-to-list 'evil-emacs-state-modes 'deadgrep-mode)

(general-define-key :keymaps '(normal motion) :prefix "SPC"
  "og" #'deadgrep)

(general-define-key :keymaps 'visual :prefix "SPC"
  "og" #'gatsby:visual-deadgrep)

(use-package envrc)
(add-hook 'after-init-hook #'envrc-global-mode)

(defun gatsby:envrc--maybe-reload ()
  "run `envrc-reload' if the current buffer name is one of \"shell.nix\", \"default.nix\", or \"flake.nix\"."
  (when (and (member (f-filename (buffer-file-name)) '("shell.nix" "default.nix" "flake.nix"))
             (member ".envrc" (--map (f-base it) (f-files default-directory))))
    (envrc-reload)))

(defun gatsby:envrc--setup-update-hook ()
  (add-hook 'after-save-hook #'gatsby:envrc--maybe-reload nil t))

(add-hook 'nix-mode-hook #'gatsby:envrc--setup-update-hook)

(advice-add #'executable-find :around #'envrc-propagate-environment)

;; mouse face for envrc-lighter
(defun gatsby:envrc--envrc-lighter ()
  "`envrc--lighter' with mouse hover showing current root directory"
  (let ((root (or (f-traverse-upwards
                   (lambda (dir) (f-exists-p (format "%s/.envrc" dir))))
                  "no direnv environment")))
    `("[e:"
      (:propertize ,(symbol-name envrc--status)
       face
       ,(pcase envrc--status
          (`on 'envrc-mode-line-on-face)
          (`error 'envrc-mode-line-error-face)
          (`none 'envrc-mode-line-none-face))
       mouse-face mode-line-highlight
       help-echo ,root
       )
      "]")))

(setq gatsby:right-mode-line `((:eval (gatsby:envrc--envrc-lighter)) " " ,@gatsby:right-mode-line))

(defun gatsby:envrc-edit-or-init ()
  "Update or init a nix-direnv environment in the current `default-directory'."
  (interactive)
  (if-let* ((root (envrc--find-env-dir)))
      (let ((shell (format "%sshell.nix" root))
            (default (format "%sdefault.nix" root))
            (flake (format "%sflake.nix" root)))
        (find-file-other-window (completing-read
                                 (format "direnv environment found at %s, openning: " root)
                                 (--filter (f-exists-p it) `(,shell ,default ,flake)))))
    (gatsby:envrc-init-direnv default-directory)))

(defun gatsby:envrc-init-direnv (root)
  "Initialize a nix-direnv environment in ROOT directory."
  (interactive (list (read-file-name "Select the root directory: "
                                     default-directory
                                     nil
                                     nil
                                     nil
                                     'directory-name-p)))
  ;; 1. put use_nix in .envrc file
  (with-temp-buffer
    (insert "use flake\n")
    (write-file (format "%s.envrc" root) nil))
  ;; 2. init (and open) a shell.nix file with basic structure
  (f-touch (format "%sflake.nix" root))
  ;; ask if want to allow
  (when (and (eq envrc--status 'error)
             (y-or-n-p (format "%s is not allowed by direnv, allowing it?")))
    (envrc-allow))
  (find-file-other-window (format "%sflake.nix" root))
  (yas-expand-snippet (yas-lookup-snippet "new-shell"))
  (evil-insert-state))

(defun gatsby:envrc-log-buffer ()
  "Switch to `*envrc*' buffer."
  (interactive)
  (with-current-buffer "*envrc*"
    (goto-char (point-max))
    (goto-char (or (re-search-backward "^====" nil t) (point-min))))
  (switch-to-buffer-other-window "*envrc*"))

(defun gatsby:envrc--unkillable-log-buffer ()
  (if (string= (buffer-name (current-buffer)) "*envrc*")
      (progn
        (delete-window (get-buffer-window "*envrc*"))
        nil)
    t))

(add-hook 'kill-buffer-query-functions #'gatsby:envrc--unkillable-log-buffer)

(add-to-list 'evil-motion-state-modes 'special-mode)

(general-define-key :keymaps 'special-mode-map :states 'motion
  "q" 'delete-window)

(general-define-key :keymaps '(normal visual motion) :prefix "SPC"
  "ne" #'gatsby:envrc-edit-or-init
  "nn" #'envrc-reload
  "nd" #'envrc-deny
  "na" #'envrc-allow
  "nl" #'gatsby:envrc-log-buffer)

(defun gatsby:dired--turn-off-line-break ()
  (visual-line-mode -1)
  (setq truncate-lines t))

(add-hook 'dired-mode-hook #'gatsby:dired--turn-off-line-break)

(add-to-list 'evil-motion-state-modes 'dired-mode)

(use-package dired :straight (:type built-in))

(setq dired-listing-switches "-lh")

(defun gatsby:dired-toggle-hide ()
  "Toggle whether to show hidden files."
  (interactive)
  (let* ((switches dired-actual-switches)
         (new-switches (if (string-match "a" dired-actual-switches)
                           (replace-regexp-in-string "a" "" dired-actual-switches)
                         (concat dired-actual-switches "a"))))
    (setq-local dired-actual-switches new-switches)
    (revert-buffer)))

(general-define-key :keymaps 'dired-mode-map :states 'motion
  "h" #'gatsby:dired-toggle-hide
  "r" #'revert-buffer)

(general-define-key :keymaps 'dired-mode-map :states 'motion
  ;; sort
  "s" 'dired-sort-toggle-or-edit

  ;; movement
  "j" 'dired-next-line
  "k" 'dired-previous-line
  "J" (lambda () (interactive) (dired-next-line 3))
  "K" (lambda () (interactive) (dired-previous-line 3))
  "<backspace>" 'dired-up-directory

  ;; files
  "<return>" 'dired-find-file
  "<M-return>" 'dired-find-file-other-window
  "f" 'find-file
  "F" 'dired-create-directory
  "t" 'dired-show-file-type
  "y" 'dired-copy-filename-as-kill)

(general-define-key :keymaps '(motion normal visual) :prefix "SPC"
  "od" (lambda () (interactive) (dired default-directory)))

(use-package dired-rainbow
  :after dired
  :config
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*"))

(use-package dired-collapse :hook (dired-mode . dired-collapse-mode))

(use-package notmuch

  :init
  (add-to-list 'evil-motion-state-modes 'notmuch-search-mode)

  :config
  (defun gatsby:notmuch-update ()
    "run mbsync -a && notmuch new"
    (interactive)
    (let ((display-buffer-overriding-action
           '(display-buffer-no-window (allow-no-window . t))))
      (async-shell-command "mbsync -a && notmuch new"))
    ;; update lighter
    (gatsby:notmuch--update-new-mails))

  :general
  (:keymaps '(normal motion)
   :prefix "SPC"
   "ms" #'notmuch-search
   "mt" #'notmuch-search-by-tag
   "mu" #'gatsby:notmuch-update
   "mc" #'notmuch-mua-new-mail))

(defvar gatsby:notmuch-new-mails 0 "number of unread mails")
(defun gatsby:notmuch--update-new-mails (&rest _)
  "Update `gatsby:notmuch-new-mails' by calling \"notmuch count\"."
  (setq gatsby:notmuch-new-mails
        (string-to-number (shell-command-to-string
                           "notmuch count 'tag:inbox AND tag:unread'"))))

(defface gatsby:notmuch-mode-line-face '((t :inherit error))
  "Face used in mode line to indicate new mails.")

(defun gatsby:notmuch--lighter ()
  (let ((mails (if (> gatsby:notmuch-new-mails 9)
                   "10+"
                 (format "%d" gatsby:notmuch-new-mails))))
    `("[M:"
      (:propertize ,mails
       face
       ,(if (zerop gatsby:notmuch-new-mails)
            nil
          'gatsby:notmuch-mode-line-face))
      "] ")))
(setq gatsby:right-mode-line `((:eval (gatsby:notmuch--lighter)) ,@gatsby:right-mode-line))

(run-at-time nil 900 #'gatsby:notmuch--update-new-mails)

(advice-add #'notmuch-bury-or-kill-this-buffer :after #'gatsby:notmuch--update-new-mails)

(defvar gatsby:notmuch-moving-in-process nil
  "Non-nil means that there is another moving process running")

(defun gatsby:notmuch-move (&rest _)
  "call afew -a -m to move mails to their designation."
  (interactive)
  (unless gatsby:notmuch-moving-in-process
    (setq gatsby:notmuch-moving-in-process t)
    (make-process
     :name "notmuch-move"
     :buffer nil
     :noquery t
     :command '("afew" "-a" "-m")
     :sentinel (lambda (process msg)
                 (unless (eq (process-status process) 'run)
                   (setq gatsby:notmuch-moving-in-process nil)
                   (let ((exit-code (process-exit-status process)))
                     (if (zerop exit-code)
                         (message "afew successfully moved mails!")
                       (message "afew exit with code %d" exit-code))))))))

(advice-add #'notmuch-refresh-this-buffer :after #'gatsby:notmuch-move)
(advice-add #'notmuch-bury-or-kill-this-buffer :after #'gatsby:notmuch-move)

(defun gatsby:notmuch--turn-off-visual-line-mode ()
  (visual-line-mode -1))

(add-hook 'notmuch-search-hook #'gatsby:notmuch--turn-off-visual-line-mode)
(add-hook 'notmuch-notmuch-tree-mode #'gatsby:notmuch--turn-off-visual-line-mode)

(setq-default notmuch-search-oldest-first nil)

(defun gatsby:notmuch-search-trash (&optional arg)
  "Add trash tag to the current messages."
  (interactive "P")
  (let ((flag-change (if arg '("+inbox" "-trash") '("-inbox" "+trash")))
        beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point)
            end (point)))
    (notmuch-search-tag flag-change beg end))
  (when (region-active-p)
    (deactivate-mark)))

(defun gatsby:notmuch-tree-trash (&optional arg)
  "Add trash tag to the current messages."
  (interactive "P")
  (let ((flag-change (if arg '("+inbox" "-trash") '("-inbox" "+trash"))))
    (notmuch-tree-tag flag-change)
    (notmuch-tree-next-matching-message)))

(defun gatsby:notmuch-show-trash (&optional arg)
  "Add trash tag to the current message."
  (interactive)
  (let ((flag-change (if arg '("+inbox" "-trash") '("-inbox" "+trash"))))
    (notmuch-show-tag flag-change)))

(general-define-key :keymaps 'notmuch-search-mode-map :states 'motion
  ;; tag
  "a" #'notmuch-search-archive-thread
  "+" #'notmuch-search-add-tag
  "-" #'notmuch-search-remove-tag
  "'" #'notmuch-search-tag-all
  "DEL" #'gatsby:notmuch-search-trash

  "c" 'notmuch-search-stash-map

  ;; filter
  "s" #'notmuch-search-filter
  "t" #'notmuch-search-filter-by-tag

  "<tab>" #'notmuch-tree-from-search-current-query
  "q" #'notmuch-bury-or-kill-this-buffer

  ;; message related
  "<return>" #'notmuch-search-show-thread
  "r" #'notmuch-search-reply-to-thread
  "R" #'notmuch-search-reply-to-thread-sender
  "f" #'notmuch-show-forward-message)

(general-define-key :keymaps 'notmuch-search-mode-map :states 'visual
  "a" #'notmuch-search-archive-thread
  "+" #'notmuch-search-add-tag
  "-" #'notmuch-search-remove-tag
  "'" #'notmuch-search-tag-all
  "DEL" #'gatsby:notmuch-search-trash)

(general-define-key :keymaps 'notmuch-search-mode-map :states 'motion :prefix "SPC"
  "r" #'notmuch-refresh-this-buffer
  "R" #'notmuch-poll-and-refresh-this-buffer
  "q" #'notmuch-bury-or-kill-this-buffer)

(add-to-list 'evil-motion-state-modes 'notmuch-tree-mode)

(defun gatsby:notmuch-tree-scroll-down-show ()
  (interactive)
  (when notmuch-tree-message-window
    (with-selected-window notmuch-tree-message-window
      (scroll-up 10))))

(defun gatsby:notmuch-tree-scroll-up-show ()
  (interactive)
  (when notmuch-tree-message-window
    (with-selected-window notmuch-tree-message-window
      (scroll-down 10))))

(defun gatsby:notmuch-tree-next-thread (&optional previous)
  "Move to the next tree in the tree view. If PREVIOUS is non-nil, move to the previous one."
  (interactive)
  (if previous
      (unless (notmuch-tree-prev-thread-in-tree)
        (forward-line 1))
    (unless (notmuch-tree-next-thread-in-tree)
      (goto-char (point-max))
      (notmuch-tree-prev-thread-in-tree))))

(defun gatsby:notmuch-tree-prev-thread ()
  (interactive)
  (gatsby:notmuch-tree-next-thread t))

(defun gatsby:notmuch-tree-show-message ()
  (interactive)
  (if (eq this-command last-command)
      ;; now the pane is split and the tree buffer has the focus
      ;; simply delete window will let the message view to have the focus
      (progn
        (setq-local notmuch-tree-message-buffer nil
                    notmuch-tree-message-window nil)
        (delete-window))
    (call-interactively #'notmuch-tree-show-message-in)))

(general-define-key :keymaps 'notmuch-tree-mode-map :states 'motion
  "<return>" #'gatsby:notmuch-tree-show-message)

(general-define-key :keymaps 'notmuch-tree-mode-map :states 'motion
  "a" #'notmuch-tree-archive-message-then-next
  "A" #'notmuch-tree-archive-thread-then-next
  "+" #'notmuch-tree-add-tag
  "-" #'notmuch-tree-remove-tag
  "'" #'notmuch-tree-tag-thread
  "DEL" #'gatsby:notmuch-tree-trash

  "c" 'notmuch-show-stash-map

  "j" #'notmuch-tree-next-message
  "k" #'notmuch-tree-prev-message
  "J" #'gatsby:notmuch-tree-next-thread
  "K" #'gatsby:notmuch-tree-prev-thread

  "C-d" #'gatsby:notmuch-tree-scroll-down-show
  "C-u" #'gatsby:notmuch-tree-scroll-up-show
  "C-e" #'gatsby:notmuch-tree-scroll-down-show
  "C-y" #'gatsby:notmuch-tree-scroll-up-show

  "q" #'notmuch-bury-or-kill-this-buffer

  ;; message related
  "r" #'notmuch-tree-reply
  "R" #'notmuch-tree-reply-sender
  "f" #'notmuch-tree-forward-message)

(general-define-key :keymaps 'notmuch-tree-mode-map :states 'motion :prefix "SPC"
  "r" #'notmuch-refresh-this-buffer)

(add-to-list 'evil-motion-state-modes 'notmuch-show-mode)

(setq mm-text-html-renderer 'gnus-w3m)

(defun gatsby:notmuch-show-ret ()
  "Synthetic <return> key.
If there is a button under `point', push it;
If there is an url under `point', visit it;
Otherwise just <return>."
  (interactive)
  (cond ((get-char-property (point) 'button) (call-interactively 'push-button))
        ((goto-address-find-address-at-point) (call-interactively 'goto-address-at-point))
        ((re-search-backward goto-address-url-regexp (line-beginning-position) t)
         (call-interactively 'goto-address-at-point))
        (t (call-interactively 'evil-ret))))

(general-define-key :keymaps 'notmuch-show-mode-map :states 'motion
  "a" #'notmuch-show-archive-message-then-next-or-next-thread
  "A" #'notmuch-show-archive-thread-then-next
  "+" #'notmuch-show-add-tag
  "-" #'notmuch-show-remove-tag
  "'" #'notmuch-show-tag-all
  "c" 'notmuch-show-stash-map

  "r" #'notmuch-show-reply
  "R" #'notmuch-show-reply-sender
  "f" #'notmuch-show-forward-open-message

  "<" #'notmuch-show-rewind
  ">" #'notmuch-show-advance
  "DEL" #'gatsby:notmuch-show-trash
  "<return>" #'gatsby:notmuch-show-ret
  "`" #'notmuch-tree-from-show-current-query

  "u" #'notmuch-show-browse-urls
  "\\" #'toggle-truncate-lines

  "q" #'notmuch-bury-or-kill-this-buffer)

(general-define-key :keymaps 'notmuch-show-mode-map :states 'motion :prefix "SPC"
  "e" #'notmuch-show-resume-message
  "r" #'notmuch-refresh-this-buffer
  "R" #'notmuch-poll-and-refresh-this-buffer
  "q" #'notmuch-bury-or-kill-this-buffer)

(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      smtpmail-debug-verbose t
      smtpmail-debug-info t
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from t)

(setq message-default-headers "Cc: \nBcc: \n"
      message-forward-as-mime nil
      message-make-forward-subject-function 'message-forward-subject-fwd)

(setq message-citation-line-format "On %e %B %Y %R, %f wrote:\n"
      message-citation-line-function #'message-insert-formatted-citation-line)

(use-package org-mime

  :general
  (:keymaps 'notmuch-message-mode-map
   :states '(normal visual motion insert emacs)
   :prefix "C-c"
   "'" #'org-mime-edit-mail-in-org-mode)

  (:keymaps 'org-mime-src-mode-map
   :prefix "C-c"
   "C-k" #'kill-buffer-and-window))

(defun gatsby:org-mime-auto-htmlize (fn &rest _)
  "Automatically apply `org-mime-htmlize' in the source buffer."
  (let ((source-buffer (marker-buffer org-mime-src--beg-marker)))
    (call-interactively fn)
    (with-current-buffer source-buffer
      (call-interactively 'org-mime-htmlize))))

(advice-add #'org-mime-edit-src-exit :around #'gatsby:org-mime-auto-htmlize)

(defun gatsby:notmuch--always-prompt-for-sender (fn &rest _)
  "When composing messages, always prompt for sender to make sure."
  (let ((current-prefix-arg 4))
    (call-interactively fn)))

(advice-add #'notmuch-mua-new-mail :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-search-reply-to-thread :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-search-reply-to-thread-sender :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-tree-reply :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-tree-reply-sender :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-show-reply :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-show-reply-sender :around #'gatsby:notmuch--always-prompt-for-sender)
(advice-add #'notmuch-show-forward-message :around #'gatsby:notmuch--always-prompt-for-sender)

(use-package org-gcal)

(defvar gatsby:gcal-started-p nil
  "Non-nil if `org-gcal-client-id' and `org-gcal-client-secret' are set")

(defun gatsby:gcal--init (&rest _)
  (when (and (not gatsby:gcal-started-p)
             (server-running-p))
    (setq gatsby:gcal-started-p t
          org-gcal-client-id (password-store-get "google-api/id")
          org-gcal-client-secret (password-store-get "google-api/secret")
          org-gcal-file-alist (--map
                               `(,it . ,(expand-file-name it (format "%s/.cal" (getenv "HOME"))))
                               user-all-mail-addresses))))

(add-hook 'server-after-make-frame-hook #'gatsby:gcal--init)

(setq org-gcal-recurring-events-mode 'nested)

(cl-defun gatsby:gcal--schedule (account start-time end-time &key title desc)
  "Schedule an event.
ACCOUNT is the calendar account this event is registered under.
START-TIME is the start time list of the event (see `decode-time' for the input format).
END-TIME is the end time list of the event (see `decode-time' for the input format).
TITLE is the event title. If it is nil, put the initial cursor at the title field of the event.
DESC is the event description. If the title field is non-nil, put the initial cursor at the description field of the event."
  (let* ((org-file (cdr (assoc account org-gcal-file-alist)))
         (desc (or desc ""))
         (desc-field (if title (concat desc "%?") desc))
         (title (or title "%?"))
         (org-tamplate-text (concat "* " title
                                    "\n:PROPERTIES:\n"
                                    ":calendar-id:\t" account
                                    "\n:END:\n:org-gcal:\n"
                                    (format-time-string "<%Y-%m-%d %H:%M>"
                                                        (apply #'encode-time start-time))
                                    "--"
                                    (format-time-string "<%Y-%m-%d %H:%M>"
                                                        (apply #'encode-time end-time))
                                    "\n\n"
                                    desc-field
                                    "\n:END:"))
         (org-capture-templates
          `(("a" "appointment"
             entry (file ,org-file)
             ,org-tamplate-text
             :empty-lines 1))))
    (org-capture t "a")))

(defun gatsby:gcal--parse-time (time-string)
  "parse TIME-STRING of format %H:%M(pm). Return (sec min hour 0 0 0)."
  (let* ((H-offset (if (string-match ".+\\(pm\\|PM\\|pM\\|Pm\\)" time-string) '(0 12) '(0 0)))
         (raw-time (->> time-string
                        (s-split ":")
                        (seq-reverse)
                        (--map (replace-regexp-in-string "[a-zA-Z]+" "" it))
                        (-map 'string-to-number))))
    `(0 ,@(cl-map 'list '+ raw-time H-offset) 0 0 0)))

(defun gatsby:gcal-select-time ()
  "Read time from the minibuffer. Return (sec min hour) of the selected time."
  (interactive)
  (let ((selectrum-should-sort nil))
    (gatsby:gcal--parse-time
     (completing-read "Select time: " (-interleave (--map (format "%s:00" it) (-iota 24))
                                                   (--map (format "%s:30" it) (-iota 24)))))))

(defun gatsby:gcal-select-duration (&optional start-time)
  "Read duration from the minibuffer. If START-TIME is non-nil, return the end-time (start-time + duration)."
  (interactive)
  (let* ((selectrum-should-sort nil)
         (duration (gatsby:gcal--parse-time
                    (completing-read "Duration: "
                                     (-interleave (--map (format "%s:15" it) (-iota 10))
                                                  (--map (format "%s:30" it) (-iota 10))
                                                  (--map (format "%s:45" it) (-iota 10))
                                                  (--map (format "%s:00" it) (-iota 10 1)))))))
    (if start-time
        (cl-map 'list '+ start-time duration)
      duration)))

(defun gatsby:gcal-select-date ()
  "Select a date from a list of next 7 dates. Return (0 0 0 day month year) of the selected time."
  (interactive)
  (let* ((selectrum-should-sort nil)
         (date-alist
          (--map
           (let ((emacs-time (time-add (current-time) (days-to-time it))))
             `(,(format-time-string "%Y-%m-%d %A" emacs-time) . ,emacs-time))
           (-iota 30)))
         (date (completing-read "Select Date: " date-alist))
         (date-sec (cdr (assoc date date-alist))))
    (if date-sec
        `(0 0 0 ,@(-select-by-indices '(3 4 5) (decode-time date-sec)))
      `(0 0 0 ,@(-map 'string-to-number (s-split "[- ]" date))))))

(defconst gatsgy:local-timezone-offset '(0 0 8 0 0 0)
  "Transformation from UTC +0 to the local time zone.")

(defconst gatsby:timezone-alist
  '(("UTC (Africa): Ouagadougou, Abidjan, Banjul, Accra, Conakry, Bissau, Monrovia, Bamako, Nouakchott, Sao_Tome, Dakar, Freetown, Lome" . (0 0 0 0 0 0))
    ("UTC (America): Danmarkshavn" . (0 0 0 0 0 0))
    ("UTC (Antarctica): Troll" . (0 0 0 0 0 0))
    ("UTC (Atlantic): Faroe, Reykjavik, Madeira, St_Helena, Canary" . (0 0 0 0 0 0))
    ("UTC (Europe): Guernsey, Dublin, Isle_of_Man, Jersey, Lisbon, London" . (0 0 0 0 0 0))
    ("UTC +01:00 (Africa): Algiers, Luanda, Porto-Novo, Douala, Bangui, Ndjamena, Brazzaville, Kinshasa, Malabo, Libreville, Casablanca, Niamey, Lagos, Ceuta, Tunis, El_Aaiun" . (0 0 1 0 0 0))
    ("UTC +01:00 (Arctic): Longyearbyen" . (0 0 1 0 0 0))
    ("UTC +01:00 (Europe): Tirane, Andorra, Vienna, Brussels, Sarajevo, Zagreb, Prague, Copenhagen, Paris, Berlin, Busingen, Gibraltar, Vatican, Budapest, Rome, Vaduz, Luxembourg, Skopje, Malta, Monaco, Podgorica, Amsterdam, Oslo, Warsaw, San_Marino, Belgrade, Bratislava, Ljubljana, Madrid, Stockholm, Zurich" . (0 0 1 0 0 0))
    ("UTC +02:00 (Africa): Gaborone, Bujumbura, Lubumbashi, Cairo, Maseru, Tripoli, Blantyre, Maputo, Windhoek, Kigali, Johannesburg, Juba, Khartoum, Mbabane, Lusaka, Harare" . (0 0 2 0 0 0))
    ("UTC +02:00 (Asia): Famagusta, Nicosia, Jerusalem, Amman, Beirut, Gaza, Hebron, Damascus" . (0 0 2 0 0 0))
    ("UTC +02:00 (Europe): Sofia, Tallinn, Helsinki, Athens, Riga, Vilnius, Chisinau, Bucharest, Kaliningrad, Kiev, Uzhgorod, Zaporozhye, Mariehamn" . (0 0 2 0 0 0))
    ("UTC +03:00 (Africa): Djibouti, Asmara, Addis_Ababa, Nairobi, Mogadishu, Dar_es_Salaam, Kampala" . (0 0 3 0 0 0))
    ("UTC +03:00 (Antarctica): Syowa" . (0 0 3 0 0 0))
    ("UTC +03:00 (Asia): Bahrain, Baghdad, Kuwait, Qatar, Riyadh, Aden" . (0 0 3 0 0 0))
    ("UTC +03:00 (Europe): Minsk, Kirov, Moscow, Volgograd, Istanbul, Simferopol" . (0 0 3 0 0 0))
    ("UTC +03:00 (Indian): Comoro, Antananarivo, Mayotte" . (0 0 3 0 0 0))
    ("UTC +03:30 (Asia): Tehran" . (0 30 3 0 0 0))
    ("UTC +04:00 (Asia): Yerevan, Baku, Tbilisi, Muscat, Dubai" . (0 0 4 0 0 0))
    ("UTC +04:00 (Europe): Astrakhan, Samara, Saratov, Ulyanovsk" . (0 0 4 0 0 0))
    ("UTC +04:00 (Indian): Mauritius, Reunion, Mahe" . (0 0 4 0 0 0))
    ("UTC +04:30 (Asia): Kabul" . (0 30 4 0 0 0))
    ("UTC +05:00 (Antarctica): Mawson" . (0 0 5 0 0 0))
    ("UTC +05:00 (Asia): Aqtau, Aqtobe, Atyrau, Oral, Qyzylorda, Karachi, Yekaterinburg, Dushanbe, Ashgabat, Samarkand, Tashkent" . (0 0 5 0 0 0))
    ("UTC +05:00 (Indian): Kerguelen, Maldives" . (0 0 5 0 0 0))
    ("UTC +05:30 (Asia): Kolkata, Colombo" . (0 30 5 0 0 0))
    ("UTC +05:45 (Asia): Kathmandu" . (0 45 5 0 0 0))
    ("UTC +06:00 (Antarctica): Vostok" . (0 0 6 0 0 0))
    ("UTC +06:00 (Asia): Dhaka, Thimphu, Urumqi, Almaty, Qostanay, Bishkek, Omsk" . (0 0 6 0 0 0))
    ("UTC +06:00 (Indian): Chagos" . (0 0 6 0 0 0))
    ("UTC +06:30 (Asia): Yangon" . (0 30 6 0 0 0))
    ("UTC +06:30 (Indian): Cocos" . (0 30 6 0 0 0))
    ("UTC +07:00 (Antarctica): Davis" . (0 0 7 0 0 0))
    ("UTC +07:00 (Asia): Phnom_Penh, Jakarta, Pontianak, Vientiane, Hovd, Barnaul, Krasnoyarsk, Novokuznetsk, Novosibirsk, Tomsk, Bangkok, Ho_Chi_Minh" . (0 0 7 0 0 0))
    ("UTC +07:00 (Indian): Christmas" . (0 0 7 0 0 0))
    ("UTC +08:00 (Asia): Brunei, Shanghai, Hong_Kong, Makassar, Macau, Kuala_Lumpur, Kuching, Choibalsan, Ulaanbaatar, Manila, Irkutsk, Singapore, Taipei" . (0 0 8 0 0 0))
    ("UTC +08:00 (Australia): Perth" . (0 0 8 0 0 0))
    ("UTC +08:45 (Australia): Eucla" . (0 45 8 0 0 0))
    ("UTC +09:00 (Asia): Jayapura, Tokyo, Pyongyang, Seoul, Chita, Khandyga, Yakutsk, Dili" . (0 0 9 0 0 0))
    ("UTC +09:00 (Pacific): Palau" . (0 0 9 0 0 0))
    ("UTC +09:30 (Australia): Darwin" . (0 30 9 0 0 0))
    ("UTC +10:00 (Antarctica): DumontDUrville" . (0 0 10 0 0 0))
    ("UTC +10:00 (Asia): Ust-Nera, Vladivostok" . (0 0 10 0 0 0))
    ("UTC +10:00 (Australia): Brisbane, Lindeman" . (0 0 10 0 0 0))
    ("UTC +10:00 (Pacific): Guam, Chuuk, Saipan, Port_Moresby" . (0 0 10 0 0 0))
    ("UTC +10:30 (Australia): Adelaide, Broken_Hill" . (0 30 10 0 0 0))
    ("UTC +11:00 (Antarctica): Casey, Macquarie" . (0 0 11 0 0 0))
    ("UTC +11:00 (Asia): Magadan, Sakhalin, Srednekolymsk" . (0 0 11 0 0 0))
    ("UTC +11:00 (Australia): Hobart, Lord_Howe, Melbourne, Sydney" . (0 0 11 0 0 0))
    ("UTC +11:00 (Pacific): Kosrae, Pohnpei, Noumea, Bougainville, Guadalcanal, Efate" . (0 0 11 0 0 0))
    ("UTC +12:00 (Asia): Anadyr, Kamchatka" . (0 0 12 0 0 0))
    ("UTC +12:00 (Pacific): Fiji, Tarawa, Kwajalein, Majuro, Nauru, Norfolk, Funafuti, Wake, Wallis" . (0 0 12 0 0 0))
    ("UTC +13:00 (Antarctica): McMurdo" . (0 0 13 0 0 0))
    ("UTC +13:00 (Pacific): Enderbury, Auckland, Fakaofo, Tongatapu" . (0 0 13 0 0 0))
    ("UTC +13:45 (Pacific): Chatham" . (0 45 13 0 0 0))
    ("UTC +14:00 (Pacific): Kiritimati, Apia" . (0 0 14 0 0 0))
    ("UTC -01:00 (America): Scoresbysund" . (0 0 -1 0 0 0))
    ("UTC -01:00 (Atlantic): Cape_Verde, Azores" . (0 0 -1 0 0 0))
    ("UTC -02:00 (America): Noronha" . (0 0 -2 0 0 0))
    ("UTC -02:00 (Atlantic): South_Georgia" . (0 0 -2 0 0 0))
    ("UTC -03:00 (America): Argentina/Buenos_Aires, Argentina/Catamarca, Argentina/Cordoba, Argentina/Jujuy, Argentina/La_Rioja, Argentina/Mendoza, Argentina/Rio_Gallegos, Argentina/Salta, Argentina/San_Juan, Argentina/San_Luis, Argentina/Tucuman, Argentina/Ushuaia, Araguaina, Bahia, Belem, Fortaleza, Maceio, Recife, Santarem, Sao_Paulo, Punta_Arenas, Santiago, Cayenne, Nuuk, Asuncion, Miquelon, Paramaribo, Montevideo" . (0 0 -3 0 0 0))
    ("UTC -03:00 (Antarctica): Palmer, Rothera" . (0 0 -3 0 0 0))
    ("UTC -03:00 (Atlantic): Stanley" . (0 0 -3 0 0 0))
    ("UTC -03:30 (America): St_Johns" . (0 -30 -3 0 0 0))
    ("UTC -04:00 (America): Anguilla, Antigua, Aruba, Barbados, La_Paz, Kralendijk, Boa_Vista, Campo_Grande, Cuiaba, Manaus, Porto_Velho, Blanc-Sablon, Glace_Bay, Goose_Bay, Halifax, Moncton, Curacao, Dominica, Santo_Domingo, Thule, Grenada, Guadeloupe, Guyana, Martinique, Montserrat, Puerto_Rico, St_Barthelemy, St_Kitts, St_Lucia, Marigot, St_Vincent, Lower_Princes, Port_of_Spain, Caracas, Tortola, St_Thomas" . (0 0 -4 0 0 0))
    ("UTC -04:00 (Atlantic): Bermuda" . (0 0 -4 0 0 0))
    ("UTC -05:00 (America): Nassau, Eirunepe, Rio_Branco, Atikokan, Iqaluit, Nipigon, Pangnirtung, Thunder_Bay, Toronto, Cayman, Bogota, Havana, Guayaquil, Port-au-Prince, Jamaica, Cancun, Panama, Lima, Grand_Turk, Detroit, Indiana/Indianapolis, Indiana/Marengo, Indiana/Petersburg, Indiana/Vevay, Indiana/Vincennes, Indiana/Winamac, Kentucky/Louisville, Kentucky/Monticello, New_York" . (0 0 -5 0 0 0))
    ("UTC -05:00 (Pacific): Easter" . (0 0 -5 0 0 0))
    ("UTC -06:00 (America): Belize, Rainy_River, Rankin_Inlet, Regina, Resolute, Swift_Current, Winnipeg, Costa_Rica, El_Salvador, Guatemala, Tegucigalpa, Bahia_Banderas, Matamoros, Merida, Mexico_City, Monterrey, Managua, Chicago, Indiana/Knox, Indiana/Tell_City, Menominee, North_Dakota/Beulah, North_Dakota/Center, North_Dakota/New_Salem" . (0 0 -6 0 0 0))
    ("UTC -06:00 (Pacific): Galapagos" . (0 0 -6 0 0 0))
    ("UTC -07:00 (America): Cambridge_Bay, Creston, Dawson, Dawson_Creek, Edmonton, Fort_Nelson, Inuvik, Whitehorse, Yellowknife, Chihuahua, Hermosillo, Mazatlan, Ojinaga, Boise, Denver, Phoenix" . (0 0 -7 0 0 0))
    ("UTC -08:00 (America): Vancouver, Tijuana, Los_Angeles" . (0 0 -8 0 0 0))
    ("UTC -08:00 (Pacific): Pitcairn" . (0 0 -8 0 0 0))
    ("UTC -09:00 (America): Anchorage, Juneau, Metlakatla, Nome, Sitka, Yakutat" . (0 0 -9 0 0 0))
    ("UTC -09:00 (Pacific): Gambier" . (0 0 -9 0 0 0))
    ("UTC -09:30 (Pacific): Marquesas" . (0 -30 -9 0 0 0))
    ("UTC -10:00 (America): Adak" . (0 0 -10 0 0 0))
    ("UTC -10:00 (Pacific): Rarotonga, Tahiti, Honolulu" . (0 0 -10 0 0 0))
    ("UTC -11:00 (Pacific): Pago_Pago, Niue, Midway" . (0 0 -11 0 0 0)))
  "An alist of (time zone representation . time offset). The format of time offset should confirm with the output format from `decode-time'.")

(defun gatsby:gcal-select-timezone ()
  "Select a time zone from `gatsby:timezone-alist', return its offset with respect to `gatsby:local-timezone-offset'."
  (interactive)
  (let* ((selectrum-should-sort nil)
         (tz (completing-read "Select TZ: " gatsby:timezone-alist)))
    (cl-map 'list '+ (cdr (assoc tz gatsby:timezone-alist)) gatsgy:local-timezone-offset)))

(defun gatsby:gcal-schedule (tz)
  "Schedule an event.
If TZ is non-nil or called with a prefix argument, schedule the event at a different time zone.
If region is active, use the selected text as the event description."
  (interactive (if current-prefix-arg
                   `(,(gatsby:gcal-select-timezone)) '((0 0 0 0 0 0))))
  (let* ((account (completing-read "Schedule an event for: " org-gcal-file-alist))
         (date (cl-map 'list '+ tz (gatsby:gcal-select-date)))
         (start-time (cl-map 'list '+ date (gatsby:gcal-select-time)))
         (end-time (gatsby:gcal-select-duration start-time))
         (desc (when (region-active-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))))
    (gatsby:gcal--schedule account start-time end-time :desc desc)))

(general-define-key :keymaps '(normal motion visual) :prefix "SPC"
  "ma" #'gatsby:gcal-schedule)

(defun gatsby:gcal-visit-file (account)
  "Visit the org file with the calendar ACCOUNT."
  (interactive (list (completing-read "Select account: " user-all-mail-addresses)))
  (find-file (expand-file-name  (format "~/.cal/%s" account))))

(general-define-key :keymaps '(normal visual motion) :prefix "SPC"
  "mf" #'gatsby:gcal-visit-file)

(setq org-gcal-remove-api-cancelled-events t)

(defun gatsby:gcal--confirm-delete-event (fn)
  "Do not bother me with answering that I really want to delete this event"
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (prompt)
               (defvar empty-history)
               (let* ((prompt (->> prompt
                                   (replace-regexp-in-string "?\n\n" ": *")
                                   (replace-regexp-in-string "\n\n" "* (y or n) ")))
                      (empty-history '())
                      (enable-recursive-minibuffers t)
                      (msg help-form)
                      (keymap (let ((map (make-composed-keymap
                                          y-or-n-p-map query-replace-map)))
                                (when help-form
                                  ;; Create a new map before modifying
                                  (setq map (copy-keymap map))
                                  (define-key map (vector help-char)
                                    (lambda ()
                                      (interactive)
                                      (let ((help-form msg)) ; lexically bound msg
                                        (help-form-show)))))
                                map))
                      (this-command this-command)
                      (str (read-from-minibuffer
                            prompt nil keymap nil
                            (or y-or-n-p-history-variable 'empty-history))))
                 (if (member str '("y" "Y")) t nil)))))
    (call-interactively fn)))

(advice-add #'org-gcal-delete-at-point :around #'gatsby:gcal--confirm-delete-event)

(defun gatsby:gcal--notify (title msg)
  (let ((msg (replace-regexp-in-string "\n" " " msg)))
    (message "%s: %s" title msg)))

(advice-add #'org-gcal--notify :override #'gatsby:gcal--notify)

(setq org-agenda-files (f-glob (expand-file-name "~/.cal/*")))

(defun gatsby:gcal--update-reminders (&rest _)
  (cl-letf (((symbol-function 'message) #'ignore))
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)))

;; at start up
(gatsby:gcal--update-reminders)

;; when scheduling events
(advice-add #'org-gcal-post-at-point :after #'gatsby:gcal--update-reminders)
(advice-add #'org-gcal-delete-at-point :after #'gatsby:gcal--update-reminders)

;; at 00:01 every day
(run-at-time "12:01am" (* 24 3600) #'gatsby:gcal--update-reminders)

(defun gatsby:org-gcal-sync (&optional skip-export silent)
  "Sync local and remote calendar(s).  If SKIP-EXPORT is non-nil, only fetch.
Set SILENT to non-nil to inhibit notifications."
  (interactive)
  (org-gcal--ensure-token)
  (when org-gcal-auto-archive
    (dolist (i org-gcal-fetch-file-alist)
      (with-current-buffer
          (find-file-noselect (cdr i))
        (org-gcal--archive-old-event))))
  (let ((up-time (org-gcal--up-time))
        (down-time (org-gcal--down-time)))
    (deferred:loop org-gcal-fetch-file-alist
      (lambda (calendar-id-file)
        (deferred:$
          (org-gcal--sync-calendar calendar-id-file skip-export silent
                                   up-time down-time)
          (deferred:nextc it
            (lambda (_)
              (unless silent
                (org-gcal--notify "Completed event fetching ."
                                  (concat "Events fetched into\n"
                                          (cdr calendar-id-file))))))

          ;; update appt.
          (deferred:nextc it
            (lambda (_)
              (gatsby:gcal--update-reminders)))

          ;; save and close file
          (deferred:nextc it
            (lambda (_)
              (with-current-buffer (car calendar-id-file)
                (save-buffer)
                (kill-buffer))
              (deferred:succeed nil))))))))

(advice-add #'org-gcal-sync :override #'gatsby:org-gcal-sync)

(use-package appt :straight (:type built-in))

(setq appt-message-warning-time 5
      appt-display-interval appt-message-warning-time)

(defun gatsby:gcal--reminder (min-to-app _new-time msg)
  (unless (listp min-to-app)
    (setq min-to-app (list min-to-app)
          msg (list msg)))
  (--each (-zip min-to-app msg)
    (alert (format "%s in %s minutes" (cdr it) (car it)) :title "Upcoming Event")))

(setq appt-display-format 'window
      appt-disp-window-function 'gatsby:gcal--reminder)

(run-at-time nil 3600 #'org-gcal-sync)
(appt-activate 1)

(use-package calfw
  :straight (:host github :repo "tumashu/emacs-calfw" :files ("calfw-org.el" "calfw.el"))
  :custom-face
  (calfw-face-title ((t (:inherit default :height 2.0))))
  (calfw-face-toolbar ((t (:inherit default))))
  (calfw-face-toolbar-button-off ((t (:inherit button))))
  (calfw-face-toolbar-button-on ((t (:inherit default :foreground "Gray50"))))

  (calfw-face-header ((t (:inherit bold))))
  (calfw-face-sunday ((t (:inherit calfw-face-header))))
  (calfw-face-saturday ((t (:inherit calfw-face-header))))
  (calfw-face-holiday ((t (:inherit italic))))
  (calfw-face-day-title ((t (:inherit default))))
  (calfw-face-select ((t (:inherit holiday))))
  (calfw-face-today ((t (:inherit highlight))))
  (calfw-face-today-title ((t (:inherit highlight))))

  :general
  (:keymaps 'calfw-calendar-mode-map
   :states 'normal
   "q" #'bury-buffer
   "r" #'calfw-refresh-calendar-buffer

   "H" #'calfw-navi-goto-week-begin-command
   "L" #'calfw-navi-goto-week-end-command
   "K" #'calfw-navi-previous-week-command
   "J" #'calfw-navi-next-week-command

   ">" #'calfw-navi-next-month-command
   "<" #'calfw-navi-previous-month-command

   "h" #'calfw-navi-previous-day-command
   "l" #'calfw-navi-next-day-command
   "b" #'calfw-navi-previous-day-command
   "w" #'calfw-navi-next-day-command
   "k" #'calfw-navi-prev-item-command
   "j" #'calfw-navi-next-item-command

   "d" #'calfw-show-details-command
   "RET" #'calfw-show-details-command
   "M" #'calfw-change-view-month
   "W" #'calfw-change-view-week
   "D" #'calfw-change-view-day
   "t" #'calfw-navi-goto-today-command
   "TAB" #'calfw-navi-next-item-command
   "<backtab>" #'calfw-navi-prev-item-command)

  (:keymaps 'calfw-details-mode-map
   :states 'normal
   "q" #'calfw-details-kill-buffer-command
   "TAB" #'calfw-details-navi-next-item-command
   "<backtab>" #'calfw-details-navi-prev-item-command))

(use-package calfw-org :straight (:type built-in))

(defconst gatsby:calfw-candidate-colors
  '("#268bd2" "#8DC85F" "#e06c75" "#e5c07b" "#ECA964" "#FF96B3" "#D1A5FF")
  "The color used in calfw to differentiate between different calendars.")

(defun gatsby:calfw-open-calendar ()
  (interactive)
  (when (functionp 'org-gcal-sync)
    (org-gcal-sync nil t))
  (let ((cp
         (calfw-create-calendar-component-buffer
          :view 'week
          :contents-sources
          (--map (calfw-org-create-file-source (caar it) (cdar it) (cdr it))
                 (-zip org-gcal-file-alist gatsby:calfw-candidate-colors)))))
    (switch-to-buffer (calfw-cp-get-buffer cp))))

(general-define-key :keymaps '(normal visual motion) :prefix "SPC"
  "mo" #'gatsby:calfw-open-calendar)

(defun gatsby:calfw--split-vertically (fn &rest _)
  (let ((split-window-preferred-function (lambda (&rest _) (split-window-below))))
    (call-interactively fn)))

(advice-add #'calfw-show-details-command :around #'gatsby:calfw--split-vertically)

(defun gatsby:calfw-schedule (&optional tz)
  "Schedule an event using `calfw-cursor-to-nearest-date' for date."
  (interactive "p")
  (let* ((account (completing-read "Schedule an event for: " org-gcal-file-alist))
         (tz (if tz (gatsby:gcal-select-timezone) '(0 0 0 0 0 0)))
         (date (cl-map 'list '+ (calfw-cursor-to-nearest-date) tz))
         (start-time (cl-map 'list '+ date (gatsby:gcal-select-time)))
         (end-time (gatsby:gcal-select-duration start-time)))
    (gatsby:gcal--schedule account start-time end-time)))

(general-define-key :keymaps 'calfw-calendar-mode-map :states 'normal
  "n" #'gatsby:calfw-schedule)

(defun gatsby:calfw-schedule-from-detail (&optional tz)
  "Schedule an event from `calfw-details-buffer' using the date of the detail buffer."
  (interactive "p")
  (let* ((account (completing-read "Schedule an event for: " org-gcal-file-alist))
         (tz (if tz (gatsby:gcal-select-timezone) '(0 0 0 0 0 0)))
         (date (save-excursion
                 (goto-char (point-min))
                 `(0 0 0 ,@(->> (thing-at-point 'line)
                                (s-split "/")
                                seq-reverse
                                (--map (replace-regexp-in-string "[a-zA-Z]+" "" it))
                                (-map 'string-to-number)))))
         (date-with-tz (cl-map 'list '+ tz (gatsby:gcal-select-date)))
         (start-time (cl-map 'list '+ date-with-tz (gatsby:gcal-select-time)))
         (end-time (gatsby:gcal-select-duration start-time)))
    (gatsby:gcal--schedule account start-time end-time)))

(general-define-key :keymaps 'calfw-details-mode-map :states 'normal
  "n" #'gatsby:calfw-schedule)

(advice-add #'calfw-details-mode :after #'org-set-font-lock-defaults)

(general-define-key :keymaps 'calfw-details-mode-map :states 'normal
  "RET" #'org-open-at-point)

(use-package slack
  :custom-face
  (slack-preview-face ((t (:inherit highlight))))
  (slack-new-message-marker-face ((t (:height 1.0 :foreground "#d33682"))))

  :general
  (:keymaps '(slack-mode-map slack-buffer-mode-map)
   :states 'normal
   "<" #'slack-buffer-goto-prev-message
   ">" #'slack-buffer-goto-next-message
   "A" (lambda () (interactive) (goto-char (point-max)) (call-interactively #'evil-append))
   "q" #'kill-buffer-and-window)

  (:keymaps '(slack-mode-map slack-buffer-mode-map slack-edit-message-mode-map)
   :state '(normal visual insert)
   :prefix "C-c"
   "m" #'slack-message-embed-mention
   "M" #'slack-message-embed-channel))

(defvar gatsby:slack-started-p nil "Non-nil if `slack-start' has been run before.")

(defun gatsby:slack--init (&rest _)
  (when (and (not gatsby:slack-started-p)
             (server-running-p))

    (slack-register-team
     :name "berkeley-econ"
     :token (password-store-get "slack/berkeley-econ")
     :subscribed-channels '(general jmc p-room)
     :full-and-display-names t
     :modeline-enabled t
     :modeline-name "CAL"
     :animate-image t)

    (slack-register-team
     :name "robinhood-university"
     :token (password-store-get "slack/robinhood-university")
     :subscribed-channels '(general)
     :full-and-display-names t
     :modeline-enabled t
     :modeline-name "RU"
     :animate-image t)

    (slack-start)
    (setq gatsby:slack-started-p t)
    ;; modeline lighter
    (add-to-list 'gatsby:right-mode-line '(:eval slack-modeline))
    ;; update every 5 minutes
    (run-at-time nil 300 #'slack-update-modeline)))

(add-hook 'server-after-make-frame-hook #'gatsby:slack--init)

(defun gatsby:slack--format-modeline (alist)
  (let* ((slack-status (-non-nil (-map #'gatsby:slack--format-modeline-1 alist)))
         (status (if slack-status (s-join "|" slack-status) "0")))
    (format "[S:%s] " status)))

(defun gatsby:slack--format-modeline-1 (elm)
  "ELM is '(team-name . ((thread . (has-unreads . mention-count)) (channel . (has-unreads . mention-count))))"
  (let* ((name (car elm))
         (summary (cdr elm))
         (thread (cdr (assq 'thread summary)))
         (channel (cdr (assq 'channel summary)))
         (unread (or (car thread) (car channel)))
         (thread-count (cdr thread))
         (channel-count (cdr channel)))
    (cond ((not unread) nil)
          ((> 0 thread-count) (propertize name 'face 'compilation-error))
          ((> 0 channel-count) (propertize name 'face 'compilation-warning)))))

(setq slack-modeline-formatter #'gatsby:slack--format-modeline)

(defun gatsby:slack-open ()
    "Open a channel, group or im within `slack-current-team'."
    (interactive)
    (let* ((team (slack-team-select 'no-default))
           (room (slack-room-select (nconc (slack-team-ims team)
                                           (slack-team-channels team)
                                           (slack-team-groups team))
                                    team)))
      (slack-room-display room team)))

(general-define-key :keymaps '(normal motion) :prefix "SPC"
 "mq" #'gatsby:slack-open)

(defun gatsby:slack--open-quote-editor (room team quote-string)
  "Open an editor for ROOM in TEAM. Put QUOTE-STRING into the editor.

Taken from `slack-room-display'."
  (cl-labels
      ((edit (buf)
             (let ((editor-buf (slack-buffer-display-message-compose-buffer buf)))
               ;; HACK: wait 0.5 sec for the buffer creation to finish
               (run-at-time 0.5 nil (lambda () (insert quote-string))))))
    (let ((buf (slack-buffer-find 'slack-message-buffer team room)))
      (if buf (edit buf)
        (message "No Message in %s, fetching from server..." (slack-room-name room team))
        (slack-room-clear-messages room)
        (slack-conversations-view
         room team
         :after-success #'(lambda (messages cursor)
                            (slack-room-set-messages room messages team)
                            (edit (slack-create-message-buffer room cursor team))))))))

(defun gatsby:slack-quote (beg end)
  "Insert the visually selected text/code into a slack message. Prompt the user to choose the team and room for the text."
  (interactive "r")
  (let* ((raw-string (filter-buffer-substring beg end))
         (quote-type-map '(("text" . (lambda (tx)
                                       (->> tx
                                            (s-split "\n")
                                            (--map (concat "> " it))
                                            (s-join "\n"))))
                           ("code" . (lambda (tx)
                                       (concat "```" tx "```")))))
         (quote-type (completing-read "quoting: " '("text" "code")))
         (quote-string (funcall (cdr (assoc-string quote-type quote-type-map)) raw-string))
         ;; select destination
         (team (slack-team-select 'no-default))
         (room (slack-room-select
                (nconc (slack-team-ims team)
                       (slack-team-channels team)
                       (slack-team-groups team))
                team)))
    (gatsby:slack--open-quote-editor room team quote-string)))

(general-define-key :keymaps 'visual :prefix "SPC"
  "mq" #'gatsby:slack-quote)

(defun gatsby:slack-edit ()
  "Edit the message at point."
  (interactive)
  (if (slack-get-ts)
      (call-interactively 'slack-message-edit)
    (call-interactively 'slack-message-write-another-buffer)))

(general-define-key :keymaps '(slack-mode-map slack-buffer-mode-map) :states 'normal :prefix "C-c"
  "'" #'slack-message-write-another-buffer)

(general-define-key :keymaps '(slack-mode-map slack-buffer-mode-map) :states 'normal :prefix "C-c"
  "r" #'slack-message-add-reaction
  "R" #'slack-message-remove-reaction)

(use-package elisp-mode :straight (:type built-in))

(defun gatsby:lisp--set-tab-width ()
  (setq-local tab-width 2))

(add-hook 'emacs-lisp-mode-hook #'gatsby:lisp--set-tab-width)

(use-package ielm
  :straight (:type built-in)
  :init
  (defun gatsby:ielm-repl ()
    "Start an ielm REPL."
    (interactive)
    (let* ((buf (get-buffer-create "*ielm*")))
      (unless (comint-check-proc buf)
        (with-current-buffer buf
          (inferior-emacs-lisp-mode)))
      buf))
  
  (add-to-list 'gatsby:comint-repl-function-alist '(emacs-lisp-mode gatsby:ielm-repl))
  (add-to-list 'gatsby:comint-repl-function-alist '(lisp-interaction-mode gatsby:ielm-repl))
  
  (add-to-list 'gatsby:comint-repl-mode-alist '(emacs-lisp-mode . inferior-emacs-lisp-mode))
  (add-to-list 'gatsby:comint-repl-mode-alist '(lisp-interaction-mode . inferior-emacs-lisp-mode))
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook #'company-mode)
  (defun gatsby:ielm--eval-input ()
    (ielm-eval-input ielm-input))
  
  (defun gatsby:ielm-return ()
    (interactive)
    (advice-add #'comint-send-input :after #'gatsby:ielm--eval-input)
    (call-interactively #'gatsby:comint-return)
    (advice-remove #'comint-send-input #'gatsby:ielm--eval-input))
  
  (general-define-key :keymaps 'inferior-emacs-lisp-mode-map :states 'insert
    "<return>" #'gatsby:ielm-return)
  (defun gatsby:lisp-eval-region-or-sexp ()
    "Evaluate the selected region if in visual state. Otherwise evaluate the sexp before point."
    (interactive)
    (if (region-active-p)
        (gatsby:comint--eval-region 'ielm-send-input (region-beginning) (region-end))
      (save-excursion
        (backward-sexp)
        (gatsby:comint--send-code-to-repl 'ielm-send-input (thing-at-point 'sexp)))))
  (general-define-key :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map) :states '(motion normal visual) :prefix "SPC"
    "rr" #'gatsby:lisp-eval-region-or-sexp
    "rb" #'gatsby:comint-eval-buffer
    "rz" #'gatsby:comint-associate-repl
    "ro" #'gatsby:comint-start-or-pop-to-repl))

(use-package helpful)

(add-to-list 'evil-motion-state-modes 'helpful-mode)

(general-define-key :keymaps 'helpful-mode-map :states 'motion :prefix "SPC"
  "q" #'kill-buffer-and-window)

(general-define-key :keymaps '(motion normal visual) :prefix "SPC"
  "hf" 'helpful-callable
  "hk" 'helpful-key
  "hv" 'helpful-variable
  "hm" 'describe-mode)

(use-package aggressive-indent)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(use-package easy-escape)

(add-hook 'emacs-lisp-mode-hook #'easy-escape-minor-mode)
(add-hook 'ielm-mode-hook #'easy-escape-minor-mode)

(use-package python
  :mode ("\\.py'" . python-mode)
  :hook
  (python-mode . gatsby:python--set-indent-width)
  (python-mode . eglot-ensure)
  (python-mode . tree-sitter-mode)
  :init
  (defun gatsby:python--set-indent-width ()
    (setq-local tab-width 4))
  (setf (cdr (assq 'python-mode eglot-server-programs)) '("pyright-langserver" "--stdio"))
  :config
  (defun gatsby:python-start-repl ()
    "Infer python versions from shebang.  If there is no shebang, promote the user for python's version."
    (let* ((shebang (save-excursion (goto-char 1) (thing-at-point 'line)))
           (kernel (if (string-match "#!/usr/bin/env\s+\\(python.?\\)" (or shebang ""))
                       (match-string 1 shebang)
                     (completing-read "Cannot infer python interpreter, please select: "
                                      '("python2" "python3")))))
      (jupyter-run-repl kernel kernel (current-buffer) nil t)))
  
  (add-to-list 'gatsby:jupyter-repl-function-alist '(python-mode . gatsby:python-start-repl))
  (defun gatsby:python--dedent-string (string)
    "remove the comment indentation of STRING."
    (let ((strings (split-string string "\n")))
      (if (cdr strings)
          ;; multiline, need to remove indentation
          (let ((indent (if (string-match "^[\t ]+" (car strings))
                            (length (match-string 0 (car strings)))
                          0)))
            (s-join "\n"
                    (--map (if (s-prefix-p (s-repeat indent " ") it)
                               (substring it indent)
                             "")
                           strings)))
        string)))
  
  (defun gatsby:python-eval-region-or-line ()
    "If region is active, eval current region, otherwise eval current line."
    (interactive)
    (let* ((region (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list (line-beginning-position) (line-end-position))))
           (string (gatsby:python--dedent-string (apply
                                                  #'buffer-substring-no-properties
                                                  region))))
      (jupyter-eval-string string)
      (deactivate-mark)))
  :general
  (:states '(motion normal visual)
   :keymaps 'python-mode-map
   :prefix "SPC"
   "rb" #'jupyter-eval-buffer
   "rr" #'gatsby:python-eval-region-or-line
   "ro" #'gatsby:jupyter-start-or-switch-to-repl
  
   "rz" #'jupyter-repl-associate-buffer
   "rZ" #'jupyter-repl-restart-kernel)
  
  (:states 'visual
   :keymaps 'python-mode-map
   [remap evil-shift-left] #'python-indent-shift-left
   [remap evil-shift-right] #'python-indent-shift-right))

(setq python-indent-offset 4
      python-indent-guess-indent-offset-verbose nil)

(use-package stata-mode
  :straight (stata-mode :repo "junyi-hou/stata-mode" :host github)
  :mode ("\\.do'" . stata-mode)
  :config
  (add-to-list 'gatsby:jupyter-repl-function-alist '(stata-mode . "stata"))
  :general
  (:states '(motion normal visual)
   :keymaps 'stata-mode-map
   :prefix "SPC"
   "rb" #'jupyter-eval-buffer
   "rr" #'jupyter-eval-line-or-region
   "ro" #'gatsby:jupyter-start-or-switch-to-repl
  
   "rz" #'jupyter-repl-associate-buffer
   "rZ" #'jupyter-repl-restart-kernel))

(use-package ess
  :hook
  (ess-r-mode . eglot-ensure)
  (ess-r-mode . tree-sitter-mode)
  (ess-r-mode . gatsby:tree-sitter--install-and-load-r)
  :config
  (add-to-list 'gatsby:jupyter-repl-function-alist '(ess-r-mode . "irkernel"))
  (defun gatsby:r-pipe ()
    "Insert tidyverse pipe symbol %>%."
    (interactive)
    (insert "%>%")
    (reindent-then-newline-and-indent))
  (defun gatsby:r-assign-or-pipe ()
    "If the last non space character before `point' is \"=\", replace the equal sign with the assign operator. Otherwise insert a pipe operator."
    (interactive)
    (if (re-search-backward "[[:space:]]*=[[:space:]]*\\=" (line-beginning-position) t)
        (progn (replace-match "") (call-interactively #'ess-insert-assign))
      (gatsby:r-pipe)))
  
  (general-define-key :keymaps 'ess-r-mode-map :states 'insert
    "M-RET" #'gatsby:r-assign-or-pipe)
  :general
  (:keymaps 'ess-r-mode-map
   :states '(motion normal visual)
   :prefix "SPC"
   "rb" #'jupyter-eval-buffer
   "rr" #'jupyter-eval-line-or-region
   "ro" #'gatsby:jupyter-start-or-switch-to-repl
  
   "rz" #'jupyter-repl-associate-buffer
   "rZ" #'jupyter-repl-restart-kernel))

(defun gatsby:tree-sitter--install-and-load-r ()
  ;; pull tree-sitter R source code
  (unless (f-exists-p (concat tree-sitter-langs-git-dir "/repos/r"))
    (let ((default-directory tree-sitter-langs-git-dir))
      (tree-sitter-langs--call
       "git" "submodule" "add" "https://github.com/r-lib/tree-sitter-r" "repos/r")))

  ;; compile using tree-sitter-cli tool
  (unless (--filter (string= (f-base it) "r") (f-entries (tree-sitter-langs--bin-dir)))
    ;; do not open a new buffer to display compile information
    (cl-letf (((symbol-function 'tree-sitter-langs--buffer) (lambda (&rest _) nil)))
      (tree-sitter-langs-compile 'r)))

  ;; register ess-r-mode with r grammar
  (unless (assq 'ess-r-mode tree-sitter-major-mode-language-alist)
    (add-to-list 'tree-sitter-major-mode-language-alist '(ess-r-mode . r)))

  ;; register tree-sitter
  (tree-sitter-require 'r))

(use-package nix-mode
  :straight (nix-mode :host github :repo "nixOS/nix-mode"
                      :files (:defaults "*.el"))
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :hook
  (nix-mode . eglot-ensure)
  ;; the order is important: `tree-sitter-mode' needs to be after
  ;; `install-and-load-nix'
  (nix-mode . tree-sitter-mode)
  (nix-mode . gatsby:tree-sitter-install-and-load-nix)
  :init
  (defun gatsby:nix-shell-in-storage (storage)
    "Open an eshell/vterm in the directory of STORAGE."
    (interactive (list (read-file-name "Select the package directory: "
                                       "/nix/store/"
                                       nil
                                       nil
                                       nil
                                       'directory-name-p)))
    (let ((default-directory storage))
      (if current-prefix-arg
          (gatsby:vterm-open-here)
        (gatsby:eshell-open-here))))
  
  (general-define-key :keymaps '(normal motion) :prefix "SPC"
    "ns" #'gatsby:nix-shell-in-storage)
  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  :config
  (add-to-list 'gatsby:comint-repl-function-alist '(nix-mode gatsby:nix-repl))
  (add-to-list 'gatsby:comint-repl-mode-alist '(nix-mode . nix-repl-mode))
  
  (defun gatsby:nix-repl ()
    (let ((buf (get-buffer-create "*Nix-REPL*")))
      (unless (comint-check-proc buf)
        (nix--make-repl-in-buffer buf)
        (with-current-buffer buf (nix-repl-mode)))
      buf))
  
  (general-define-key :keymaps 'nix-mode-map :states '(normal visual) :prefix "SPC"
    "ro" #'gatsby:comint-start-or-pop-to-repl
    "rr" #'gatsby:comint-eval-region-or-line
    "rb" #'gatsby:comint-eval-buffer
    "rz" #'gatsby:comint-associate-nix-repl)
  (setq nix-nixfmt-bin "nixpkgs-fmt")
  
  (defun gatsby:nix--fmt-before-save ()
    (add-hook 'before-save-hook #'nix-format-buffer nil t))
  
  (add-hook 'nix-mode-hook #'gatsby:nix--fmt-before-save)
  (defun gatsby:nix--save-excrusion (fn &rest args)
    (let ((ori-point (point)))
      (apply fn args)
      (goto-char ori-point)))
  
  (advice-add #'nix-format-buffer :around #'gatsby:nix--save-excrusion))

(defun gatsby:tree-sitter-install-and-load-nix ()
  "Download, compile, and register nix grammar for tree-sitter if haven't done so."

  (unless (f-exists-p (concat tree-sitter-langs-git-dir "/repos/nix"))
    (let ((default-directory tree-sitter-langs-git-dir))
      (tree-sitter-langs--call
       "git" "submodule" "add" "https://github.com/cstrahan/tree-sitter-nix" "repos/nix")))

  (unless (--filter (string= (f-base it) "nix") (f-entries (tree-sitter-langs--bin-dir)))
    ;; do not open a new buffer to display compile information
    (cl-letf (((symbol-function 'tree-sitter-langs--buffer) (lambda (&rest _) nil)))
      (tree-sitter-langs-compile 'nix)))

  (unless (assq 'nix-mode tree-sitter-major-mode-language-alist)
    (add-to-list 'tree-sitter-major-mode-language-alist '(nix-mode . nix)))

  (tree-sitter-require 'nix))

(use-package markdown-mode)

(use-package man
  :hook
  (man-mode . outline-minor-mode)

  :general
  (:keymaps 'Man-mode-map
   :states 'motion
   "zc" #'outline-hide-entry
   "zo" #'outline-show-entry
   ">" #'outline-next-heading
   "<" #'outline-previous-heading)

  :config
  (defun gatsby:man--maybe-use-current-window (fn &rest args)
    "Use the current window to display the man buffer if in eshell mode. Respect `Man-notify-method' otherwise."
    (if (eq major-mode 'eshell-mode)
        (cl-letf (((symbol-function 'Man-notify-when-ready) 'switch-to-buffer))
          (apply fn args))
      (apply fn args)))
  
  (advice-add #'man :around #'gatsby:man--maybe-use-current-window))

(use-package tex
  :straight auctex
  :custom
  (tex-fontify-script nil)
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 1.0)
  :custom-face
  (font-latex-italic-face ((t (:underline nil :inherit 'italic))))
  (font-latex-slide-title-face ((t (:height 1.0 :inherit 'font-lock-function-name-face))))
  (font-latex-sectioning-5-face ((t (:inherit 'default))))
  :hook
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . eglot-ensure)
  (LaTeX-mode . company-mode)
  (LaTeX-mode . gatsby:latex--setup)
  :init
  (defun gatsby:latex--setup ()
    "Setup tab width and indentation rule for `LaTeX-mode'."
    (setq-local tab-width 2)
    (add-hook 'before-save-hook
              (lambda () (indent-region (point-min) (point-max))) nil t))
  :config
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        LaTeX-command "latex -shell-escape")
  
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-outline-extra '(("^\\(% \\)?\\\\begin{frame}" 3)))
  (setq TeX-electric-math (cons "$" "$")
        LaTeX-electric-left-right-brace t
        LaTeX-syntactic-comments nil)
  
  (defun gatsby:latex-delete ()
    "Deleting the whole pair if in an empty pair, other wise delete the character on the left."
    (interactive)
    (let* ((old-point (point))
           (TeX-braces-association `(,@TeX-braces-association
                                     ("{" . "}")
                                     ("\\[" . "\\]")))
           (lbrace-pos (re-search-backward (rx (group-n 1 (= 1 (or "{" "[" "(")))
                                               (0+ space))
                                           nil t))
           lbrace)
      (if (not lbrace-pos)
          (call-interactively 'backward-delete-char-untabify)
        ;; deal with { and \{
        (setq lbrace (match-string 1))
        (when (TeX-escaped-p)
          (setq lbrace (concat TeX-esc lbrace))
          (backward-char))
        (let* ((rbrace (cdr (assoc lbrace TeX-braces-association)))
               (lmacro-regexp `(rx (= 1 ,TeX-esc (group-n 1 (or ,@(-map 'car LaTeX-left-right-macros-association)))) point))
               (lmacro-pos (re-search-backward (eval lmacro-regexp) nil t))
               (brace-start (point))
               lmacro rmacro)
          (if lmacro-pos
              (setq lmacro (match-string 1)
                    rmacro (cdr (assoc lmacro LaTeX-left-right-macros-association)))
            (setq rmacro ""
                  lmacro ""))
          (if (re-search-forward (eval `(rx point
                                            (opt ,TeX-esc)
                                            ,lmacro ,lbrace
                                            (0+ space)
                                            (opt ,TeX-esc)
                                            ,rmacro ,rbrace))
                                 nil t)
              (delete-region brace-start (point))
            (goto-char old-point)
            (call-interactively 'backward-delete-char-untabify))))))
  
  (general-define-key :keymaps 'LaTeX-mode-map :states 'insert
    "DEL" #'gatsby:latex-delete)
  (setq LaTeX-syntactic-comments nil
        LaTeX-item-indent 0)
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  :general
  (:keymaps 'LaTeX-mode-map :states 'normal :prefix "SPC"
    "rr" (lambda ()
           (interactive)
           (TeX-command-sequence '("LaTeX" "BibTeX" "LaTeX" "LaTeX" "View") t))
    "ro" (lambda ()
           (interactive)
           (TeX-command-sequence "View" t)))
  (:keymaps 'LaTeX-mode-map :states 'normal
   "<" #'outline-previous-heading
   ">" #'outline-next-heading))

(defun gatsby:switch-home ()
  "Make an asynchronous process tangling, building and switch to `home-manager' configuration defined in `flake.nix'. Print stdout to `*flake-process*' buffer.

The exact configuration to be built is determined by the output of \"uname\". This flake currently support x86_64-linux and x86_64-darwin platforms.
"
  (interactive)
  (when (process-live-p "flake-process")
    (user-error "Another flake process is currently running, exiting"))

  (if (buffer-live-p (get-buffer "*flake-process*"))
      (with-current-buffer "*flake-process*"
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))))
    (with-current-buffer (get-buffer-create "*flake-process*")
      (special-mode)))

  (let ((exec-path `(,default-directory ,@exec-path)))
    (make-process
     :name "flake-process"
     :buffer "*flake-process*"
     :command '("update-user-config")
     :filter
     (lambda (proc msg)
       (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (let ((moving (= (point) (process-mark proc)))
                 (inhibit-read-only t))
             (save-excursion
               (goto-char (process-mark proc))
               (insert (xterm-color-filter msg))
               (set-marker (process-mark proc) (point)))
             (if moving (goto-char (process-mark proc)))))))))

  (pop-to-buffer "*flake-process*"))
