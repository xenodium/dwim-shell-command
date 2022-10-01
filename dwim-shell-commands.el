;;; dwim-shell-commands.el --- Useful commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/dwim-shell-command

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

;; A collection of useful commands created via
;; `dwim-shell-command-on-marked-files'.

;;; Code:

(require 'cl-lib)
(require 'dwim-shell-command)
(require 'files)
(require 'seq)
(require 'subr-x)

(defun dwim-shell-commands-audio-to-mp3 ()
  "Convert all marked audio to mp3(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to mp3"
   "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
   :utils "ffmpeg"))

(defun dwim-shell-commands-open-clipboard-url ()
  "Open clipboard URL.  Offer to stream if possible."
  (interactive)
  (let ((url (or (current-kill 0)
                 (user-error "Nothing in clipboard"))))
    (dwim-shell-commands-url-browse url)))

(defun dwim-shell-commands-url-browse (url &rest args)
  "If URL is playable media, offer to open in mpv.  Else browser.
Optional argument ARGS as per `browse-url-default-browser'"
  (if (and (or (string-match-p "^http[s]?://.*youtube.com" url)
               (string-match-p "^http[s]?://.*m.youtube.com" url)
               (string-match-p "^http[s]?://.*youtu.be" url)
               (string-match-p "^http[s]?://.*soundcloud.com" url)
               (string-match-p "^http[s]?://.*redditmedia.com" url)
               (string-match-p "^http[s]?://.*reddit.com" url)
               (string-match-p "^http[s]?://.*bandcamp.com" url))
           (y-or-n-p "Stream from mpv? "))
      (dwim-shell-command-on-marked-files
       "Streaming"
       (format "mpv --geometry=30%%x30%%+100%%+0%% '%s'" url)
       :utils "mpv"
       :no-progress t
       :error-autofocus t
       :silent-success t)
    (funcall #'browse-url-default-browser url args)))

(defun dwim-shell-commands-stream-clipboard-url ()
  "Stream clipboard URL using mpv."
  (interactive)
  (cl-assert (string-match-p "^http[s]?://" (current-kill 0)) nil "Not a URL")
  (dwim-shell-command-on-marked-files
   "Streaming"
   "mpv --geometry=30%x30%+100%+0% \"<<cb>>\""
   :utils "mpv"
   :no-progress t
   :error-autofocus t
   :silent-success t))

(defun dwim-shell-commands-download-clipboard-stream-url ()
  "Download clipboard URL."
  (interactive)
  (cl-assert (string-match-p "^http[s]?://" (current-kill 0)) nil "Not a URL")
  (dwim-shell-command-on-marked-files
   "Downloading"
   "youtube-dl --newline -o \"~/Downloads/%(title)s.%(ext)s\" \"<<cb>>\""
   :utils "youtube-dl"
   :no-progress t
   :error-autofocus t
   :monitor-directory "~/Downloads"
   :silent-success t))

(defun dwim-shell-commands-image-to-jpg ()
  "Convert all marked images to jpg(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to jpg"
   "convert -verbose '<<f>>' '<<fne>>.jpg'"
   :utils "convert"))

(defun dwim-shell-commands-image-to-png ()
  "Convert all marked images to png(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to png"
   "convert -verbose '<<f>>' '<<fne>>.png'"
   :utils "convert"))

(defun dwim-shell-commands-svg-to-png ()
  "Convert all marked svg(s) to png(s)"
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to png"
   "rsvg-convert -b white <<f>> -f png -o <<fne>>.png"
   :utils "rsvg-convert"))

(defun dwim-shell-commands-make-transparent-png ()
  "Create a transparent png."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Create transparent png"
   "convert -verbose -size <<width:200>>x<<height:200>> xc:none '<<empty<<width:200>>x<<height:200>>.png(u)>>'"
   :utils "convert"))

(defun dwim-shell-commands-join-as-pdf ()
  "Join all marked images as a single pdf."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Join as pdf"
   (format "convert -verbose '<<*>>' '%s'"
           (dwim-shell-command-read-file-name
            "Join as pdf named (default \"joined.pdf\"): "
            :extension "pdf"
            :default "<<joined.pdf(u)>>"))
   :utils "convert"))

(defun dwim-shell-commands-image-to-grayscale ()
  "Convert all marked images to grayscale."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert image to grayscale"
   "convert -verbose -type Grayscale '<<f>>' '<<fne>>_grayscale.<<e>>'"
   :utils "convert"))

(defun dwim-shell-commands-reorient-image ()
  "Reorient images."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Reorient image"
   "convert -verbose -auto-orient '<<f>>' '<<fne>>_reoriented.<<e>>'"
   :utils "convert"))

(defun dwim-shell-commands-video-to-gif ()
  "Convert all marked videos to gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to gif"
   "ffmpeg -loglevel quiet -stats -y -i '<<f>>' -pix_fmt rgb24 -r 15 '<<fne>>.gif'"
   :utils "ffmpeg"))

(defun dwim-shell-commands-video-to-webp ()
  "Convert all marked videos to webp(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to webp"
   "ffmpeg -i '<<f>>' -vcodec libwebp -filter:v fps=fps=10 -compression_level 3 -lossless 1 -loop 0 -preset default -an -vsync 0 '<<fne>>'.webp"
   :utils "ffmpeg"))

(defun dwim-shell-commands-video-to-optimized-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to optimized gif"
   "ffmpeg -loglevel quiet -stats -y -i '<<f>>' -pix_fmt rgb24 -r 15 '<<fne>>.gif'
    gifsicle -O3 '<<fne>>.gif' --lossy=80 -o '<<fne>>.gif'"
   :utils '("ffmpeg" "gifsicle")))

(defun dwim-shell-commands-unzip ()
  "Unzip all marked archives (of any kind) using `atool'."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Unzip" "atool --extract --explain '<<f>>'"
   :utils "atool"))

(defun dwim-shell-commands-speed-up-gif ()
  "Speeds up gif(s)."
  (interactive)
  (let ((factor (string-to-number
                 (completing-read "Speed up x times: " '("1" "1.5" "2" "2.5" "3" "4")))))
    (dwim-shell-command-on-marked-files
     "Speed up gif"
     (format "gifsicle -U '<<f>>' <<frames>> -O2 -o '<<fne>>_x%s.<<e>>'" factor)
     :extensions "gif" :utils '("gifsicle" "identify")
     :post-process-template (lambda (script file)
                              (string-replace "<<frames>>" (dwim-shell-commands--gifsicle-frames-every factor file) script)))))

(defun dwim-shell-commands-resize-gif ()
  "Resize marked gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Resize marked gif(s)"
   "gifsicle --scale <<Scaling factor:0.5>> '<<f>>' -o '<<fne>>_x<<Scaling factor:0.5>>.gif'"
   :extensions "gif"
   :utils "gifsicle"))

(defun dwim-shell-commands-epub-to-org ()
  "Convert epub(s) to org."
  (interactive)
  (dwim-shell-command-on-marked-files
   "epub to org"
   "pandoc --from=epub --to=org '<<f>>' > '<<fne>>.org'"
   :extensions "epub"
   :utils "pandoc"))

(defun dwim-shell-commands-docx-to-pdf ()
  "Convert docx(s) to pdf (via latex)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "docx to pdf (via latex)"
   "pandoc -t latex '<<f>>' -o '<<fne>>.pdf'"
   :extensions "docx" ;; brew install mactex
   :utils "pdflatex"))

(defun dwim-shell-commands-pdf-to-txt ()
  "Convert pdf to txt."
  (interactive)
  (dwim-shell-command-on-marked-files
   "pdf to txt"
   "pdftotext -layout '<<f>>' '<<fne>>.txt'"
   :utils "pdftotext"))

(defun dwim-shell-commands-resize-image ()
  "Resize marked image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to gif"
   (let ((factor (read-number "Resize scaling factor: " 0.5)))
     (format "convert -resize %%%d '<<f>>' '<<fne>>_x%.2f.<<e>>'"
             (* 100 factor) factor))
   :utils "convert"))

(defun dwim-shell-commands-pdf-password-protect ()
  "Speeds up gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Password protect pdf"
   (format "qpdf --verbose --encrypt '%s' '%s' 256 -- '<<f>>' '<<fne>>_enc.<<e>>'"
           (read-passwd "user-password: ")
           (read-passwd "owner-password: "))
   :utils "qpdf"
   :extensions "pdf"))

(defun dwim-shell-commands--gifsicle-frames-every (skipping-every file)
  "Generate frames SKIPPING-EVERY count for video FILE."
  (string-join
   (seq-map (lambda (n) (format "'#%d'" n))
            (number-sequence 0 (string-to-number
                                ;; Get total frames count.
                                (seq-first (process-lines "identify" "-format" "%n\n" file)))
                             skipping-every)) " "))

(defun dwim-shell-commands-drop-video-audio ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Drop audio"
   "ffmpeg -i '<<f>>' -c copy -an '<<fne>>_no_audio.<<e>>'"
   :utils "ffmpeg"))

(defun dwim-shell-commands-ping-google ()
  "Ping duckduckgo.com."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Ping google.com"
   "ping -c 3 google.com"
   :utils "ping"
   :focus-now t))

(defun dwim-shell-commands-speed-up-video ()
  "Speed up video(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Speed up video"
   (let ((factor (read-number "Resize scaling factor: " 2)))
     (format "ffmpeg -i '<<f>>' -an -filter:v 'setpts=%s*PTS' '<<fne>>_x%s.<<e>>'"
             (/ 1 (float factor)) factor))
   :utils "ffmpeg"))

(defun dwim-shell-commands-resize-video ()
  "Resize marked images."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to gif"
   "
eval $(ffprobe -v quiet -show_format -of flat=s=_ -show_entries stream=width '<<f>>');
width=${streams_stream_0_width};
zmodload zsh/mathfunc
width=$((rint($width * <<Scaling factor:0.5>>)));
# Make it even or face 'not divisible by 2' errors.
if [[ $((width%2)) -ne 0 ]] then
  width=$(($width - 1))
fi
ffmpeg -n -i '<<f>>' -vf \"scale=$width:-2\" '<<fne>>_x<<Scaling factor:0.5>>.<<e>>'"
   :utils "ffmpeg"))

(defun dwim-shell-commands-bin-plist-to-xml ()
  "Convert binary plist to xml."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert binary plist to xml"
   "plutil -convert xml1 -o '<<fne>>.xml' '<<f>>'"
   :utils "plutil"))

(defun dwim-shell-commands-clipboard-to-qr ()
  "Generate a QR code from clipboard."
  (interactive)
  (let ((temp-file (concat (temporary-file-directory) "qr-code")))
    (dwim-shell-command-on-marked-files
     "Generate a QR code from clipboard"
     (format "qrencode -s10 -o %s %s" temp-file (shell-quote-argument (current-kill 0)))
     :utils "qrencode"
     :on-completion (lambda (buffer)
                      (kill-buffer buffer)
                      (switch-to-buffer (find-file-noselect temp-file t))))))

(defun dwim-shell-commands-macos-hardware-overview ()
  "View macOS hardware overview."
  (interactive)
  (dwim-shell-command-on-marked-files
   "macOS hardware overview"
   "system_profiler SPHardwareDataType"
   :utils "system_profiler"))

(defun dwim-shell-commands-macos-toggle-display-rotation ()
  "View macOS hardware overview."
  (interactive)
  ;; #  Display_ID    Resolution  ____Display_Bounds____  Rotation
  ;; 2  0x2b347692    1440x2560      0     0  1440  2560    270    [main]
  ;; From fb-rotate output, get the `current-rotation' from Column 7, row 1 zero-based.
  (let ((current-rotation (nth 7 (split-string (nth 1 (process-lines "fb-rotate" "-i"))))))
    (dwim-shell-command-on-marked-files
     "macOS hardware overview"
     (format "fb-rotate -d 1 -r %s" (if (equal current-rotation "270") "0" "270"))
     :utils "fb-rotate")))

(defun dwim-shell-commands-files-combined-size ()
  "Get files combined file size."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Get files combined file size"
   "du -csh <<*>>"
   :utils "du"
   :on-completion (lambda (buffer)
                    (with-current-buffer buffer
                      (message "Total size: %s"
                               (progn
                                 (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                                 (match-string 1))))
                    (kill-buffer buffer))))

(defun dwim-shell-commands-image-to-icns ()
  "Convert png to icns icon."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert png to icns icon."
   "
    # Based on http://stackoverflow.com/questions/12306223/how-to-manually-create-icns-files-using-iconutil
    # Note: png must be 1024x1024
    mkdir '<<fne>>.iconset'
    sips -z 16 16 '<<f>>' --out '<<fne>>.iconset/icon_16x16.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_16x16@2x.png'
    sips -z 32 32 '<<f>>' --out '<<fne>>.iconset/icon_32x32.png'
    sips -z 64 64 '<<f>>' --out '<<fne>>.iconset/icon_32x32@2x.png'
    sips -z 128 128 '<<f>>' --out '<<fne>>.iconset/icon_128x128.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_128x128@2x.png'
    sips -z 256 256 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_512x512.png'
    sips -z 512 512 '<<f>>' --out '<<fne>>.iconset/icon_256x256@2x.png'
    sips -z 1024 1024 '<<f>>' --out '<<fne>>.iconset/icon_512x512@2x.png'
    iconutil -c icns '<<fne>>.iconset'"
   :utils '("sips" "iconutil")
   :extensions "png"))

(defun dwim-shell-commands-git-clone-clipboard-url-to-downloads ()
  "Clone git URL in clipboard to \"~/Downloads/\"."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat download-dir (file-name-base url)))
         (default-directory download-dir))
    (when (or (not (file-exists-p project-dir))
              (when (y-or-n-p (format "%s exists.  delete?" (file-name-base url)))
                (delete-directory project-dir t)
                t))
      (dwim-shell-command-on-marked-files
       (format "Clone %s" (file-name-base url))
       (format "git clone %s" url)
       :utils "git"
       :on-completion (lambda (buffer)
                        (kill-buffer buffer)
                        (dired project-dir))))))

(defun dwim-shell-commands-git-clone-clipboard-url ()
  "Clone git URL in clipboard to `default-directory'."
  (interactive)
  (dwim-shell-command-on-marked-files
   (format "Clone %s" (file-name-base (current-kill 0)))
   "git clone <<cb>>"
   :utils "git"))

(defun dwim-shell-commands-install-iphone-device-ipa ()
  "Install iPhone device .ipa.
Needs ideviceinstaller and libmobiledevice installed."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Install .ipa"
   "ideviceinstaller -i '<<f>>'"
   :utils "ideviceinstaller"))

(defun dwim-shell-commands-copy-to-downloads ()
  "Copy file to ~/Downloads."
  (interactive)
  (dwim-shell-command-foreach
   (lambda (file)
     (copy-file file "~/Downloads/" 1)
     (file-name-concat "~/Downloads" (file-name-nondirectory file)))
   :monitor-directory "~/Downloads"))

(defun dwim-shell-commands-move-to-downloads ()
  "Move file to ~/Downloads."
  (interactive)
  (dwim-shell-command-foreach
   (lambda (file)
     (rename-file file "~/Downloads/" 1)
     (when (buffer-file-name)
       (rename-buffer (file-name-nondirectory file))
       (set-visited-file-name
        (file-name-concat "~/Downloads" (file-name-nondirectory file)))
       (set-buffer-modified-p nil))
     (file-name-concat "~/Downloads" (file-name-nondirectory file)))
   :monitor-directory "~/Downloads"))

(defun dwim-shell-commands-copy-to-desktop ()
  "Copy file to ~/Desktop."
  (interactive)
  (dwim-shell-command-foreach
   (lambda (file)
     (copy-file file "~/Desktop/" 1)
     (file-name-concat "~/Desktop" (file-name-nondirectory file)))
   :monitor-directory "~/Desktop"))

(defun dwim-shell-commands-move-to-desktop ()
  "Move file to ~/Desktop."
  (interactive)
  (dwim-shell-command-foreach
   (lambda (file)
     (rename-file file "~/Desktop/" 1)
     (when (buffer-file-name)
       (rename-buffer (file-name-nondirectory file))
       (set-visited-file-name
        (file-name-concat "~/Desktop" (file-name-nondirectory file)))
       (set-buffer-modified-p nil))
     (file-name-concat "~/Desktop" (file-name-nondirectory file)))
   :monitor-directory "~/Desktop"))

(provide 'dwim-shell-commands)

;;; dwim-shell-commands.el ends here
