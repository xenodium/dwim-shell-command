;;; dwim-shell-command.el --- Shell commands with DWIM behaviour -*- lexical-binding: t; -*-

;; Author: Alvaro Ramirez
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/xenodium/dwim-shell-command
;; Version: 0.1

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `dwim-shell-command' as an opinionated DWIM alternative to
;; `shell-command'.
;;
;; Use `dwim-shell-command-on-marked-files' to create your own command
;; line utilities, invoked via M-x.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'dired-aux)
(require 'map)
(require 'seq)
(require 'shell)
(require 'subr-x)
(require 'view)

(defvar dwim-shell-command--commands nil "All commands in progress.")

(cl-defstruct
    dwim-shell-command--command
  "Describes a command in progress."
  script
  process
  name
  calling-buffer
  reporter
  on-completion
  files-before
  silent-success)

(defun dwim-shell-command ()
  "Execute DWIM shell command asynchronously using noweb templates.

Which files

  `dwim-shell-command' attempts to guess which file(s) you may want
  the command to operate on.

  1. If visiting a `dired' buffer, draw the marked file(s).
  2. If visiting any other buffer with an associated file, use that.

Templates

  Operate on drawn files using either the following:

    <<f>> (file path)
    <<fne>> (file path without extension)
    <<e>> (extension)
    <<td>> (generate a temporary directory)
    <<*>> (all files joined)

  For example:

    With drawn files '(\"path/to/image1.png\" \"path/to/image2.png\")

   \"convert '<<f>>' '<<fne>>.jpg'\" expands to

     \"convert 'path/to/image1.png' 'path/to/image1.jpg'\"
     \"convert 'path/to/image2.png' 'path/to/image2.jpg'\"

   while \"ls -lh <<*>>\" expands to

     \"ls -lh path/to/image1.png path/to/image2.png\"

Focus

  `dwim-shell-command' creates a process buffer to capture command
  output, but doesn't display or focus on it by default.  Instead,
  it tries to guess what's more convenient to focus on.

  While the process is busy, show a spinner in the minibuffer.  No
  focus changes.

  After process is finished:

  1. If there were any files created in the `default-directory',
  jump to a `dired' buffer and move point to the new file (via
  `dired-jump').

  2. If no new files were created, automatically switch focus to the
  process buffer and display its output.

    Note: You can prevent this automatic focus by prepending your
    command with whitespace.

      |
      V
    \" convert '<<f>>' '<<fne>>.jpg'\"

  3. If the shell command caused any errors, offer to focus the
  process buffer and display its output.

Quick exit

  Process buffers are read-only and can be quickly closed by
  pressing `q'."
  (interactive)
  (dwim-shell-command-on-marked-files
   "DWIM shell command" (read-shell-command "DWIM shell command: ")))

(cl-defun dwim-shell-command-on-marked-files (buffer-name script &key utils extensions shell-util shell-args shell-pipe post-process-template on-completion)
  "Create DWIM utilities executing templated SCRIPT on given files.

Here's a simple utility invoking SCRIPT to convert image files to jpg.

  (defun dwim-shell-command-convert-image-to-jpg ()
    \"Convert all marked images to jpg(s).\"
    (interactive)
    (dwim-shell-command-on-marked-files
     \"Convert to jpg\"
     \"convert -verbose '<<f>>' '<<fne>>.jpg'\"
     :utils \"convert\"))

Check `dwim-shell-command-commands.el' for more examples.

All command process output is written to a buffer with BUFFER-NAME.

If SCRIPT starts with whitespace, imply `:SILENT-SUCCESS' in
 `dwim-shell-command-execute-script'.

All params explained in `dwim-shell-command-execute-script'.

Which files

  `dwim-shell-command-on-marked-files' attempts to guess which file(s)
  you may want the command to operate on.

  1. If visiting a `dired' buffer, draw the marked file(s).
  2. If visiting any other buffer with an associated file, use that.

Templates

  Operate on drawn files using either the following:

    <<f>> (file path)
    <<fne>> (file path without extension)
    <<e>> (extension)
    <<td>> (generate a temporary directory)
    <<*>> (all files joined)

  For example:

    With drawn files '(\"path/to/image1.png\" \"path/to/image2.png\")

   \"convert '<<f>>' '<<fne>>.jpg'\" expands to

     \"convert 'path/to/image1.png' 'path/to/image1.jpg'\"
     \"convert 'path/to/image2.png' 'path/to/image2.jpg'\"

   while \"ls -lh <<*>>\" expands to

     \"ls -lh path/to/image1.png path/to/image2.png\"

Focus

  `dwim-shell-command-on-marked-files' creates a process buffer to
  capture command output, but doesn't display or focus on it by
  default.  Instead, it tries to guess what's more convenient to focus
  on.

  While the process is busy, show a spinner in the minibuffer.  No
  focus changes.

  After process is finished:

  1. If there were any files created in the `default-directory',
  jump to a `dired' buffer and move point to the new file (via
  `dired-jump').

  2. If no new files were created, automatically switch focus to the
  process buffer and display its output.

    Note: You can prevent this automatic focus by prepending your
    command with whitespace.

      |
      V
    \" convert '<<f>>' '<<fne>>.jpg'\"

  3. If the shell command caused any errors, offer to focus the
  process buffer and display its output.

Quick exit

  Process buffers are read-only and can be quickly closed by
  pressing `q'."
  (dwim-shell-command-execute-script buffer-name script
                                     :files (dwim-shell-command--marked-files)
                                     :utils utils
                                     :extensions extensions
                                     :shell-util shell-util
                                     :shell-args shell-args
                                     :shell-pipe shell-pipe
                                     :post-process-template post-process-template
                                     :on-completion on-completion
                                     :silent-success (string-prefix-p " " script)))

(cl-defun dwim-shell-command-execute-script (buffer-name script &key files extensions shell-util shell-args shell-pipe utils post-process-template on-completion silent-success gen-temp-dir)
  "Execute a script asynchronously, DWIM style with SCRIPT and BUFFER-NAME.

:FILES are used to instantiate SCRIPT as a noweb template.

  The following are supported:

    <<f>> (file path)
    <<fne>> (file path without extension)
    <<e>> (extension)
    <<td>> (generate a temporary directory)
    <<*>> (all files joined)

  For example:

    Given :FILES '(\"path/to/image1.png\" \"path/to/image2.png\")

    \"convert '<<f>>' '<<fne>>.jpg'\" expands to

      \"convert 'path/to/image1.png' 'path/to/image1.jpg'\"
      \"convert 'path/to/image2.png' 'path/to/image2.jpg'\"

    and \"ls -lh <<*>>\" expands to

      \"ls -lh path/to/image1.png path/to/image2.png\"

:EXTENSIONS ensures that all files in :FILES have the given
extensions.  Can be either single string \"png\" or a list '(\"png\" \"jpg\").

:SHELL-UTIL and :SHELL-ARGS can be used to specify SCRIPT interpreter.

  For python, use:

    (dwim-shell-command-execute-script
       \"Print Pi\"
       \"import math
         print math.pi\"
       :shell-util \"python\"
       :shell-args \"-c\")

:SHELL-PIPE can be used to pipe SCRIPT to it

  For swift, use:

    (dwim-shell-command-on-marked-files
       \"Print Pi\"
       \"print(Double.pi)\"
       :shell-pipe \"swift -\")

:UTILS ensures that all needed command line utilities are installed.
Can be either a single string \"ffmpeg\" or a list '(\"ffmpet\" \"convert\").

:POST-PROCESS-TEMPLATE enables processing template further after noweb
instantiation.

:ON-COMPLETION is invoked after SCRIPT executes (disabling DWIM
internal behavior).

:SILENT-SUCCESS to avoid jumping to process buffer if neither error
 nor file generated."
  (cl-assert buffer-name nil "Script must have a buffer name")
  (cl-assert (not (string-empty-p script)) nil "Script must not be empty")
  (when (stringp extensions)
    (setq extensions (list extensions)))
  (when (and shell-util (stringp shell-util))
    (setq shell-util (list shell-util)))
  (when (and shell-args (stringp shell-args))
    (setq shell-args (list shell-args)))
  (when (stringp utils)
    (setq utils (list utils)))
  (when (or gen-temp-dir (string-match-p "\<\<td\>\>" script 0))
    (setq gen-temp-dir (make-temp-file "dwim-shell-command-" t)))
  (when (seq-empty-p files)
    (cl-assert (not (or (dwim-shell-command--contains-multi-file-ref script)
                        (dwim-shell-command--contains-single-file-ref script)))
               nil "No files found to expand %s"
               (or (dwim-shell-command--contains-multi-file-ref script)
                   (dwim-shell-command--contains-single-file-ref script))))
  (when extensions
    (seq-do (lambda (file)
              (cl-assert (seq-contains-p extensions (downcase (file-name-extension file)))
                         nil "Not a .%s file" (string-join extensions " .")))
            files))
  (seq-do (lambda (util)
            (cl-assert (executable-find util) nil (format "%s not installed" util)))
          utils)
  (let* ((proc-buffer (generate-new-buffer (format "*%s*" buffer-name)))
         (template script)
         (script "")
         (files-before)
         (proc)
         (progress-reporter))
    (if (seq-empty-p files)
        (setq script template)
      (if (dwim-shell-command--contains-multi-file-ref template)
          (setq script (dwim-shell-command--expand-files-template template files post-process-template gen-temp-dir))
        (seq-do (lambda (file)
                  (setq script
                        (concat script "\n"
                                (dwim-shell-command--expand-file-template template file post-process-template gen-temp-dir))))
                files)))
    (setq script (string-trim script))
    (with-current-buffer proc-buffer
      (require 'shell)
      (shell-mode))
    (with-current-buffer proc-buffer
      (setq default-directory default-directory)
      (shell-command-save-pos-or-erase)
      (view-mode +1)
      (setq view-exit-action 'kill-buffer))
    (setq files-before (dwim-shell-command--default-directory-files))
    (setq proc (apply 'start-process (seq-concatenate 'list
                                                      (list (buffer-name proc-buffer) proc-buffer)
                                                      (or shell-util '("zsh"))
                                                      (or shell-args '("-x" "-c"))
                                                      (if shell-pipe
                                                          (list (format "echo '%s' | %s" script shell-pipe))
                                                        (list script)))))
    (setq progress-reporter (make-progress-reporter (process-name proc)))
    (progress-reporter-update progress-reporter)
    ;; Momentarily set buffer to same window, so it's next in recent stack.
    ;; Makes finding the shell command buffer a lot easier.
    (let ((current (current-buffer)))
      (pop-to-buffer-same-window proc-buffer)
      (pop-to-buffer-same-window current))
    (if (equal (process-status proc) 'exit)
        (progn
          (dwim-shell-command--finalize (current-buffer)
                                        files-before
                                        proc
                                        progress-reporter
                                        on-completion
                                        silent-success))
      (setq dwim-shell-command--commands
            (push (cons (process-name proc)
                        (make-dwim-shell-command--command :script script
                                                          :process proc
                                                          :name (process-name proc)
                                                          :calling-buffer (current-buffer)
                                                          :files-before files-before
                                                          :reporter progress-reporter
                                                          :on-completion on-completion
                                                          :silent-success silent-success))
                  dwim-shell-command--commands))
      (set-process-sentinel proc #'dwim-shell-command--sentinel)
      (set-process-filter proc #'dwim-shell-command--filter))))

(defun dwim-shell-command--expand-files-template (template files &optional post-process-template temp-dir)
  "Expand TEMPLATE using FILES.

Expand using <<*>> for FILES.

Note: This expander cannot be used to expand <<f>>, <<fne>>, or <<e>>.

  For example:

    Given FILES '(\"path/to/image1.png\" \"path/to/image2.png\")

    \"du -csh <<*>>\" expands to

      \"du -csh 'path/to/image1.png' 'path/to/image2.png'\"

Use POST-PROCESS-TEMPLATE to further expand template given own logic.

Set TEMP-DIR to a unique temp directory to this template."
  (cl-assert (not (and (dwim-shell-command--contains-multi-file-ref template)
                       (dwim-shell-command--contains-single-file-ref template)))
             nil "Must not have %s and %s in the same template"
             (dwim-shell-command--contains-multi-file-ref template)
             (dwim-shell-command--contains-single-file-ref template))
  (setq files (seq-map (lambda (file)
                         (expand-file-name file))
                       files))
  ;; "<<*>>" with '("path/to/image1.png" "path/to/image2.png") -> "path/to/image1.png path/to/image2.png"
  (setq template (replace-regexp-in-string "\\(\<\<\\*\>\>\\)" (string-join files " ") template nil nil 1))

  ;; "<<td>>" with TEMP-DIR -> "/var/folders/m7/ky091cp56d5g68nyhl4y7frc0000gn/T/dwim-shell-command-JNK4V5"
  (setq template (replace-regexp-in-string "\\(\<\<td\>\>\\)" temp-dir template nil nil 1))

  (when post-process-template
    (setq template (funcall post-process-template template files)))
  template)

(defun dwim-shell-command--expand-file-template (template file &optional post-process-template temp-dir)
  "Expand TEMPLATE using FILE.

Expand using <<f>> for FILE, <<fne>> for FILE without extension, and
 <<e>> for FILE extension.

Note: This expander cannot be used to expand <<*>>, <<fne>>, or <<e>>.

  For example:

    Given FILE \"path/to/image.png\"

    \"convert '<<f>>' '<<fne>>.jpg'\" expands to

      \"convert 'path/to/image.png' 'path/to/image.jpg'\"

Use POST-PROCESS-TEMPLATE to further expand template given own logic.

Set TEMP-DIR to a unique temp directory to this template."
  (cl-assert (not (and (dwim-shell-command--contains-multi-file-ref template)
                       (dwim-shell-command--contains-single-file-ref template)))
             nil "Must not have %s and %s in the same template"
             (dwim-shell-command--contains-multi-file-ref template)
             (dwim-shell-command--contains-single-file-ref template))
  (setq file (expand-file-name file))
  ;; "<<fne>>" with "/path/tmp.txt" -> "/path/tmp"
  (setq template (replace-regexp-in-string "\\(\<\<fne\>\>\\)" (file-name-sans-extension file) template nil nil 1))

  ;; "<<e>>" with "/path/tmp.txt" -> "txt"
  (setq template (replace-regexp-in-string "\\(\<\<e\>\>\\)" (file-name-extension file) template nil nil 1))

  ;; "<<f>>" with "/path/file.jpg" -> "/path/file.jpg"
  (setq template (replace-regexp-in-string "\\(\<\<f\>\>\\)" file template nil nil 1))

  ;; "<<td>>" with TEMP-DIR -> "/var/folders/m7/ky091cp56d5g68nyhl4y7frc0000gn/T/dwim-shell-command-JNK4V5"
  (setq template (replace-regexp-in-string "\\(\<\<td\>\>\\)" temp-dir template nil nil 1))

  (when post-process-template
    (setq template (funcall post-process-template template file)))
  template)

(defun dwim-shell-command--contains-single-file-ref (template)
  "Check for <<f>>, <<fne>>, or <<e>> in TEMPLATE."
  (cond ((string-match "\<\<f\>\>" template)
         "<<f>>")
        ((string-match "\<\<fne\>\>" template)
         "<<fne>>")
        ((string-match "\<\<e\>\>" template)
         "<<e>>")))

(defun dwim-shell-command--contains-multi-file-ref (template)
  "Check for <<*>> in TEMPLATE."
  (when (string-match "\<\<\\*\>\>" template)
    "<<*>>"))

(defun dwim-shell-command--default-directory-files ()
  "List of files in current buffer's `default-directory'."
  (when default-directory
    (seq-map (lambda (filename)
               (concat default-directory filename))
             (process-lines "ls" "-1"))))

(defun dwim-shell-command--last-modified-between (before after)
  "Compare files in BEFORE and AFTER and return oldest file in diff."
  (car (last (seq-sort #'file-newer-than-file-p
                       (seq-difference after before)))))

(defun dwim-shell-command--finalize (calling-buffer files-before process progress-reporter on-completion silent-success)
  "Finalize script execution.

 CALLING-BUFFER, FILES-BEFORE, PROCESS, PROGRESS-REPORTER, and
ON-COMPLETION SILENT-SUCCESS are all needed to finalize processing."
  (let ((oldest-new-file))
    (when progress-reporter
      (progress-reporter-done progress-reporter))
    (if (= (process-exit-status process) 0)
        (if on-completion
            (progn
              (funcall on-completion (process-buffer process)))
          (progn
            (with-current-buffer calling-buffer
              (when (and (equal major-mode 'dired-mode)
                         revert-buffer-function)
                (funcall revert-buffer-function nil t))
              (setq oldest-new-file
                    (dwim-shell-command--last-modified-between
                     files-before
                     (dwim-shell-command--default-directory-files)))
              (when oldest-new-file
                (dired-jump nil oldest-new-file)))
            (unless (equal (process-buffer process)
                           (window-buffer (selected-window)))
              (if oldest-new-file
                  (kill-buffer (process-buffer process))
                (unless silent-success
                  (switch-to-buffer (process-buffer process)))))))
      (if (and (buffer-name (process-buffer process))
               (y-or-n-p (format "Couldn't run %s, see output? " (buffer-name (process-buffer process)))))
          (switch-to-buffer (process-buffer process))
        (kill-buffer (process-buffer process))))
    (setq dwim-shell-command--commands
          (map-delete dwim-shell-command--commands (process-name process)))))

(defun dwim-shell-command--sentinel (process _)
  "Handles PROCESS sentinel and STATE."
  (let ((exec (map-elt dwim-shell-command--commands (process-name process))))
    (dwim-shell-command--finalize (dwim-shell-command--command-calling-buffer exec)
                                  (dwim-shell-command--command-files-before exec)
                                  process
                                  (dwim-shell-command--command-reporter exec)
                                  (dwim-shell-command--command-on-completion exec)
                                  (dwim-shell-command--command-silent-success exec))))

(defun dwim-shell-command--filter (process output)
  "Handles PROCESS filtering and STATE and OUTPUT."
  (when-let* ((exec (map-elt dwim-shell-command--commands (process-name process)))
              (reporter (dwim-shell-command--command-reporter exec)))
    (progress-reporter-update reporter))
  (comint-output-filter process output))

(defun dwim-shell-command--marked-files ()
  "Return buffer file (if available) or marked files for a `dired' buffer."
  (if (buffer-file-name)
      (list (buffer-file-name))
    (dired-get-marked-files)))

(provide 'dwim-shell-command)

;;; dwim-shell-command.el ends here
