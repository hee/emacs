;; 隔離された環境にパッケージをインストール
; (例：パッケージを指定して起動)　emacs -q -l ~/path/to/somewhere/init.el
;
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (url-retrieve
   "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
	   (goto-char (point-max))
	   (eval-print-last-sexp))))

;;; Emacsのバージョンによってインストール先を変える
(let ((versioned-dir (locate-user-emacs-file emacs-version)))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
	package-user-dir (expand-file-name "elpa" versioned-dir)))

;(when (eq system-type 'darwin)
;    (setq ns-command-modifier (quote meta)))

