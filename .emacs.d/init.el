;; 隔離された環境にパッケージをインストール
; (例：パッケージを指定して起動)　emacs -q -l ~/path/to/somewhere/init.el
;
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))



;;; base setting
(setq line-number-mode t) ;;カーソル行番号表示
(setq inhibit-startup-message t) ;;スタートアップメッセージ非表示
(setq make-backup-files nil) ;;バックアップファイルを作らない
(setq frame-title-format "%b") ;;タイトルにファイル名表示
(auto-compression-mode t) ;;日本語info文字化け防止
(global-hl-line-mode) ;;行ハイライト
(show-paren-mode 1) ;;対応括弧ハイライト
(setq-default line-spacing 0) ;;行間設定
(setq-default tab-width 2 indent-tabs-mode nil) ;;; インデント時にタブを使わないでスペースを使う

;
; あらゆるmajor modeを使えるようにする
;
(require 'generic-x)

;;; 行番号を表示する
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

;
; el-get 設定
;
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;(el-get 'sync)


;;; Emacsのバージョンによってインストール先を変える
;(let ((versioned-dir (locate-user-emacs-file emacs-version)))
;  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
;	  package-user-dir (expand-file-name "elpa" versioned-dir)))

;(when (eq system-type 'darwin)
;    (setq ns-command-modifier (quote meta)))



;
; 利用パッケージ設定
;

; auto-complete
(el-get-bundle auto-complete)

; color theme
(el-get-bundle color-theme-railscasts)

; howm
(el-get-bundle howm)

; markdown-mode
(el-get-bundle markdown-mode)

; php-mode
(el-get-bundle php-mode)

; projectile-rails
(el-get-bundle projectile-rails)



;;;
;;; howm 設定
;;;
(setq howm-menu-lang 'ja)
;; howmメモの保存場所
(setq howm-directory "~/share/howm")
;; howm-mode
(when (require 'howm nil t)
  ;; C-c,,でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;;;
;;; markdown-mode
;;;
;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.txt" . gfm-mode) auto-mode-alist))

;; ファイル内容を標準入力で渡すのではなく、ファイル名を引数として渡すように設定
;(defun markdown-custom ()
;  "markdown-mode-hook"
;  (setq markdown-command-needs-filename t)
;  )
;(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))


;;;
;;; php-mode
;;;
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . php-mode))
(setq php-search-url "http://jp.php.net/ja/")
(setq php-manual-url "http://jp.php.net/manual/ja/")
