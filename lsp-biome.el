;;; lsp-biome.el --- A lsp-mode client for Biome -*- lexical-binding:t -*-

;; Copyright (C) 2024-present CHEN Xian'an (a.k.a `realazy').

;; Author: realazy <xianan.chen@gmail.com>
;; Keywords: biome
;; Version: 0.1
;; Package-Requires: ((lsp-mode "8.0.0"))
;; URL: https://github.com/cxa/lsp-biome

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'lsp-mode)

(defgroup lsp-biome nil
  "Adds lsp-mode support for biome."
  :group 'lsp-mode
  :link '(url-link "https://github.com/cxa/lsp-biome"))

(defcustom lsp-biome-active-file-types '("\\.[tj]s[x]?\\'" "\\.json\\'")
  "File types that lsp-biome should activate."
  :type '(repeat regexp)
  :group 'lsp-biome)

(defcustom lsp-biome-organize-imports-on-save nil
  "Enable/disable auto organize imports on save."
  :type 'boolean
  :group 'lsp-biome)

(defcustom lsp-biome-autofix-on-save nil
  "Enable/disable auto fix on save."
  :type 'boolean
  :group 'lsp-biome)

(defcustom lsp-biome-format-on-save nil
  "Enable/disable auto format on save."
  :type 'boolean
  :group 'lsp-biome)

(defvar lsp-biome--bin-path nil)
(defvar lsp-biome--activated-p nil)
(defvar lsp-biome--orig-org-imports (symbol-function 'lsp-organize-imports))

(defun lsp-biome--has-config-file ()
  "Check if there is a biome config file exists."
  (let ((root (lsp-workspace-root)))
    (or
     ;; First check if `biome.json' in workspace root
     (file-exists-p (f-join root "biome.json"))
     ;; If not check the parent directories because we might be inside
     ;; a monorepo
     (locate-dominating-file root "biome.json"))))

(defun lsp-biome--file-can-be-activated (filename)
  (seq-some (lambda (filetype) (string-match filetype filename))
            lsp-biome-active-file-types))

(defun lsp-biome--activate-p (filename &optional _)
  "Check if biome language server can/should start. Currently we only
support projects that installed `biome'."
  (when-let* ((wroot (lsp-workspace-root))
              (broot (locate-dominating-file
                      wroot
                      "node_modules/@biomejs/biome/bin/biome"))
              (bin (apply
                    #'f-join
                    `(,broot ,@(split-string
                                "node_modules/@biomejs/biome/bin/biome" "/"))))
              ((lsp-biome--has-config-file))
              ((lsp-biome--file-can-be-activated filename)))
    (setq lsp-biome--bin-path bin)
    ;; Enploy `apheleia-mode' with a biome formatter if available
    (when (bound-and-true-p apheleia-mode)
      (unless (alist-get 'lsp-biome--formatter apheleia-formatters)
        (push '(lsp-biome--formatter
                . ("apheleia-npx" "biome" "format" "--stdin-file-path" filepath))
              apheleia-formatters))
      (setq-local apheleia-formatter '(lsp-biome--formatter)))
    t))

(lsp-make-interactive-code-action biome-organize-imports
                                  "source.organizeImports.biome")
(lsp-make-interactive-code-action biome-fix-all "source.fixAll.biome")

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     (setq lsp-biome--activated-p t)
                     `(,lsp-biome--bin-path "lsp-proxy")))
  :activation-fn #'lsp-biome--activate-p
  :server-id 'biome
  :priority -1
  :add-on? t))

(with-eval-after-load 'lsp-mode
  (defun lsp-biome--organize-imports-before-save ()
    ;; action may be unavailable, in that case we ignore the noisy error
    (ignore-error lsp-no-code-actions
      (lsp-biome-organize-imports)))

  (defun lsp-biome--autofix-before-save ()
    ;; action may be unavailable, in that case we ignore the noisy error
    (ignore-error lsp-no-code-actions
      (lsp-biome-fix-all)))

  (defun lsp-biome--should-add-save-hook-p ()
    (or lsp-biome-format-on-save
        lsp-biome-organize-imports-on-save
        lsp-biome-autofix-on-save))

  (defun lsp-biome--before-save-hook ()
    (when lsp-biome-organize-imports-on-save
      (lsp-biome--organize-imports-before-save))
    (when lsp-biome-autofix-on-save
      (lsp-biome--autofix-before-save))
    (when lsp-biome-format-on-save
      ;; Use `lsp-format-buffer' only if `apheleia-mode' is
      ;; unavailable. Note that this may cause slow unexpected
      ;; multiple edits applying (ref:
      ;; https://github.com/emacs-lsp/lsp-mode/issues/2446)
      ;; You also need to set below to avoid above behavior
      ;; (setq lsp-enable-on-type-formatting nil)
      ;; (setq lsp-enable-indentation nil)
      (unless (bound-and-true-p apheleia-mode)
        (lsp-format-buffer))))

  (defun lsp-biome--workspace-p (workspace)
    (eq (lsp--client-server-id (lsp--workspace-client workspace))
        'biome))

  (add-hook 'lsp-after-initialize-hook
            (defun lsp-biome--after-init ()
              (when (and (lsp-biome--workspace-p lsp--cur-workspace)
                         lsp-biome--activated-p)
                (defalias 'lsp-organize-imports #'lsp-biome-organize-imports)
                (when (lsp-biome--should-add-save-hook-p)
                  (add-hook 'before-save-hook
                            #'lsp-biome--before-save-hook nil t)))))
  
  (add-hook 'lsp-after-uninitialized-functions
            (defun lsp-biome--after-uninit (workspace)
              (when (and (lsp-biome--workspace-p workspace)
                         lsp-biome--activated-p)
                (defalias 'lsp-organize-imports lsp-biome--orig-org-imports)
                (remove-hook 'before-save-hook #'lsp-biome--before-save-hook t))
              (setq lsp-biome--activated-p nil))))

(provide 'lsp-biome)
;;; lsp-biome.el ends here
