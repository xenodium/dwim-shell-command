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

(require 'browse-url)
(require 'cl-lib)
(require 'dwim-shell-command)
(require 'files)
(require 'proced)
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

(defun dwim-shell-commands-image-clear-exif-metadata ()
  "Clear EXIF metadata in image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "View EXIF"
   "cp '<<f>>' '<<fne>>_cleared.<<e>>'
    exiftool -all:all= -overwrite_original '<<fne>>_cleared.<<e>>'"
   :utils "exiftool"))

(defun dwim-shell-commands-image-exif-metadata ()
  "View EXIF metadata in image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "View EXIF"
   "exiftool '<<f>>'"
   :utils "exiftool"))

(defun dwim-shell-commands-ocr-text-from-image ()
  "Extract text from image via tesseract."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Extract text from image via tesseract."
   "tesseract '<<f>>' -"
   :utils "tesseract"))

(defun dwim-shell-commands-image-browse-location ()
  "Open image(s) location in browser."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Browse location"
   "lat=\"$(exiftool -csv -n -gpslatitude -gpslongitude '<<f>>' | tail -n 1 | cut -s -d',' -f2-2)\"
    if [ -z \"$lat\" ]; then
      echo \"no latitude\"
      exit 1
    fi
    lon=\"$(exiftool -csv -n -gpslatitude -gpslongitude '<<f>>' | tail -n 1 | cut -s -d',' -f3-3)\"
    if [ -z \"$lon\" ]; then
      echo \"no longitude\"
      exit 1
    fi
    if [[ $OSTYPE == darwin* ]]; then
      open \"http://www.openstreetmap.org/?mlat=${lat}&mlon=${lon}&layers=C\"
    else
      xdg-open \"http://www.openstreetmap.org/?mlat=${lat}&mlon=${lon}&layers=C\"
    fi"
   :utils "exiftool"
   :error-autofocus t
   :silent-success t))

(defun dwim-shell-commands-image-reverse-geocode-location ()
  "Reverse geocode image(s) location."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Reverse geocode"
   "lat=\"$(exiftool -csv -n -gpslatitude -gpslongitude '<<f>>' | tail -n 1 | cut -s -d',' -f2-2)\"
    if [ -z \"$lat\" ]; then
      echo \"no latitude\"
      exit 1
    fi
    lon=\"$(exiftool -csv -n -gpslatitude -gpslongitude '<<f>>' | tail -n 1 | cut -s -d',' -f3-3)\"
    if [ -z \"$lon\" ]; then
      echo \"no longitude\"
      exit 1
    fi
    json=$(curl \"https://nominatim.openstreetmap.org/reverse?format=json&accept-language=en&lat=${lat}&lon=${lon}&zoom=18&addressdetails=1\")
    echo \"json_start $json json_end\""
   :utils '("exiftool" "curl")
   :silent-success t
   :error-autofocus t
   :on-completion (lambda (buffer _process)
                    (with-current-buffer buffer
                      (goto-char (point-min))
                      (let ((matches '()))
                        (while (re-search-forward "^json_start\\(.*?\\)json_end" nil t)
                          (push (match-string 1) matches))
                        (message "%s" (string-join (seq-map (lambda (json)
                                                              (map-elt (json-parse-string json :object-type 'alist) 'display_name))
                                                            matches)
                                                   "\n")))
                      (kill-buffer buffer)))))

(defun dwim-shell-commands-image-horizontal-flip ()
  "Horizontally flip image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Image horizontal flip"
   "convert -verbose -flop '<<f>>' '<<fne>>_h_flipped.<<e>>'"
   :utils "convert"))

(defun dwim-shell-commands-image-vertical-flip ()
  "Horizontally flip image(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Image vertical flip"
   "convert -verbose -flip '<<f>>' '<<fne>>_v_flipped.<<e>>'"
   :utils "convert"))

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
  "Convert all marked svg(s) to png(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to png"
   "rsvg-convert -b white '<<f>>' -f png -o '<<fne>>.png'"
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
   (format "convert -verbose '<<*>>' '<<%s(u)>>'"
           (dwim-shell-command-read-file-name
            "Join as pdf named (default \"joined.pdf\"): "
            :extension "pdf"
            :default "joined.pdf"))
   :utils "convert"))

(defun dwim-shell-commands-join-images-horizontally ()
  "Join all marked images horizontally as a single image."
  (interactive)
  (let ((filename (format "joined.%s"
                          (or (seq-first (dwim-shell-command--file-extensions)) "png"))))
    (dwim-shell-command-on-marked-files
     "Join images horizontally"
     (format "convert -verbose '<<*>>' +append '<<%s(u)>>'"
             (dwim-shell-command-read-file-name
              (format "Join as image named (default \"%s\"): " filename)
              :default filename))
     :utils "convert")))

(defun dwim-shell-commands-join-images-vertically ()
  "Join all marked images vertically as a single image."
  (interactive)
  (let ((filename (format "joined.%s"
                          (or (seq-first (dwim-shell-command--file-extensions)) "png"))))
    (dwim-shell-command-on-marked-files
     "Join images vertically"
     (format "convert -verbose '<<*>>' -append '<<%s(u)>>'"
             (dwim-shell-command-read-file-name
              (format "Join as image named (default \"%s\"): " filename)
              :default filename))
     :utils "convert")))

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

(defun dwim-shell-commands-video-to-hevc-mkv ()
  "Convert all marked videos to hevc mkv."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert video to h265 "
   "REPO_DIR=/tmp/other_video_transcoding
    if ! [ -d \"$REPO_DIR\" ]
    then
      git clone https://github.com/donmelton/other_video_transcoding.git $REPO_DIR
    fi
    pushd $REPO_DIR
    git pull origin master || echo \"skipping repo update...\"
    popd
    ruby $REPO_DIR/bin/other-transcode --hevc '<<f>>'"
   :utils '("git" "ffmpeg" "mkvtoolnix" "mpv")))

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

(defun dwim-shell-commands-optimize-gif ()
  "Convert all marked videos to optimized gif(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to optimized gif"
   "gifsicle -O3 '<<f>>' --lossy=90 -o '<<fne>>_optimized.gif'"
   :utils '("ffmpeg" "gifsicle")))

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

(defun dwim-shell-commands-clip-round-rect-gif ()
  "Clip gif(s) with round rectangle."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Clip round rect gif(s)"
   "width=$(ffprobe -v error -select_streams v:0 -show_entries stream=width -of default=noprint_wrappers=1:nokey=1 '<<f>>')
    height=$(ffprobe -v error -select_streams v:0 -show_entries stream=height -of default=noprint_wrappers=1:nokey=1 '<<f>>')
    convert -quiet -size \"${width}x${height}\" xc:none -fill black -draw \"roundRectangle 0,0,${width},${height} <<Width width:27>>,<<Width width:27>>\" '<<td>>/mask.png'
    convert  '<<f>>' -coalesce -background black -alpha remove -alpha off '<<td>>/no_alpha.<<e>>'
    # https://stackoverflow.com/a/66990135
    convert '<<td>>/no_alpha.<<e>>' -quiet -coalesce -alpha extract null: \\( '<<td>>/mask.png' -alpha extract \\) -compose multiply -layers composite '<<td>>/alpha.gif'
    convert '<<td>>/no_alpha.<<e>>' null: '<<td>>/alpha.gif' -quiet -alpha off -compose copy_opacity -layers composite '<<fne>>_rounded.<<e>>'
    # Turn looping on.
    mogrify -loop 0 '<<fne>>_rounded.<<e>>'
    gifsicle -O3  '<<fne>>_rounded.<<e>>' --lossy=80 -o '<<fne>>_rounded.<<e>>'"
   :extensions "gif"
   :utils '("ffprobe" "convert")))

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

(defun dwim-shell-commands-kill-process ()
  "Select and kill process."
  (interactive)
  (let* ((pid-width 5)
         (comm-width 25)
         (user-width 10)
         (processes (proced-process-attributes))
         (candidates
          (mapcar (lambda (attributes)
                    (let* ((process (cdr attributes))
                           (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                           (user (format (format "%%-%ds" user-width)
                                         (truncate-string-to-width
                                          (map-elt process 'user) user-width nil nil t)))
                           (comm (format (format "%%-%ds" comm-width)
                                         (truncate-string-to-width
                                          (map-elt process 'comm) comm-width nil nil t)))
                           (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                           (args (map-elt process 'args)))
                      (cons (if args
                                (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                              (format "%s %s %s" pid user comm))
                            process)))
                  processes))
         (selection (map-elt candidates
                             (completing-read "kill process: "
                                              (seq-sort
                                               (lambda (p1 p2)
                                                 (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                               (nth 2 (split-string (string-trim (car p2))))))
                                               candidates) nil t)))
         (prompt-title (format "%s %s %s"
                               (map-elt selection 'pid)
                               (map-elt selection 'user)
                               (map-elt selection 'comm))))
    (when (y-or-n-p (format "Kill %s?" prompt-title))
      (dwim-shell-command-on-marked-files
       (format "Kill %s" prompt-title)
       (format "kill -9 %d" (map-elt selection 'pid))
       :utils "kill"
       :error-autofocus t
       :silent-success t))))

(defun dwim-shell-commands-macos-toggle-bluetooth-device-connection ()
  "Toggle Bluetooth device connection."
  (interactive)
  (let* ((devices (seq-filter
                   (lambda (line)
                     ;; Keep lines like: af-8c-3b-b1-99-af - Device name
                     (string-match-p "^[0-9a-f]\\{2\\}" line))
                   (with-current-buffer (get-buffer-create "*BluetoothConnector*")
                     (erase-buffer)
                     ;; BluetoothConnector exits with 64 if no param is given.
                     ;; Invoke with no params to get a list of devices.
                     (unless (eq 64 (call-process "BluetoothConnector" nil (current-buffer)))
                       (kill-buffer (current-buffer))
                       (error (buffer-string)))
                     (let ((lines (split-string (buffer-string) "\n")))
                       (kill-buffer (current-buffer))
                       lines))))
         (candidates (mapcar (lambda (device)
                               ;; key (device name) : value (address)
                               (cons (nth 1 (split-string device " - "))
                                     (nth 0 (split-string device " - "))))
                             devices))
         (selected-name (completing-read "Toggle connection: "
                                         (seq-sort #'string-lessp candidates) nil t))
         (address (map-elt candidates selected-name)))
    (dwim-shell-command-on-marked-files
     (format "Toggle %s" selected-name)
     (format "BluetoothConnector %s --notify" address)
     :utils "BluetoothConnector"
     ;; :error-autofocus t
     ;; :silent-success t
     )))

(defun dwim-shell-commands-macos-bin-plist-to-xml ()
  "Convert binary plist to xml."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert binary plist to xml"
   "plutil -convert xml1 -o '<<fne>>.xml' '<<f>>'"
   :utils "plutil"))

(defun dwim-shell-commands-macos-toggle-dark-mode ()
  "Toggle macOS dark mode."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Toggle dark mode"
   "dark-mode"
   :utils "dark-mode" ;; brew install dark-mode
   :silent-success t))

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
   "Resize image"
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

(defun dwim-shell-commands-video-to-mp3 ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Convert to mp3"
   "ffmpeg -i '<<f>>' -vn -ab 128k -ar 44100 -y '<<fne>>.mp3'"
   :utils "ffmpeg"))

(defun dwim-shell-commands-video-trim-beginning ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Trim beginning"
   "ffmpeg -i '<<f>>' -y -ss <<Seconds:5>> -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
   :silent-success t
   :utils "ffmpeg"))

(defun dwim-shell-commands-video-trim-end ()
  "Drop audio from all marked videos."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Trim beginning"
   "ffmpeg -sseof -<<Seconds:5>> -i '<<f>>' -y -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
   :silent-success t
   :utils "ffmpeg"))

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
   "Resize video"
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

(defun dwim-shell-commands-clipboard-to-qr ()
  "Generate a QR code from clipboard."
  (interactive)
  (let ((temp-file (concat (temporary-file-directory) "qr-code")))
    (dwim-shell-command-on-marked-files
     "Generate a QR code from clipboard"
     (format "qrencode -s10 -o %s %s" temp-file (shell-quote-argument (current-kill 0)))
     :utils "qrencode"
     :on-completion (lambda (buffer _process)
                      (kill-buffer buffer)
                      (switch-to-buffer (find-file-noselect temp-file t))))))

(defun dwim-shell-commands-open-externally ()
  "Open file(s) externally."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Open externally"
   (if (eq system-type 'darwin)
       "open '<<f>>'"
     "xdg-open '<<f>>'")
   :silent-success t
   :utils (if (eq system-type 'darwin)
              "open"
            "xdg-open")))

(defun dwim-shell-commands-macos-caffeinate ()
  "Invoke caffeinate to prevent mac from sleeping."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Caffeinate"
   "caffeinate"
   :utils "caffeinate"
   :no-progress t
   :focus-now t))

(defun dwim-shell-commands-macos-make-finder-alias ()
  "Make macOS Finder alias."
  (interactive)
  (let ((files (dwim-shell-command--files))
        (target-dir (read-directory-name "Select target dir: " "/Applications" nil t)))
    (dwim-shell-command-on-marked-files
     "Make macOS alias"
     (format "osascript -e 'tell application \"Finder\" to make alias file to POSIX file \"<<f>>\" at POSIX file \"%s\"'"
             target-dir)
     :utils "osascript"
     :no-progress t
     :silent-success t
     :on-completion (lambda (buffer _process)
                      (kill-buffer buffer)
                      (dired-jump nil (file-name-concat target-dir (file-name-nondirectory (nth 0 files))))))))

(defun dwim-shell-commands-macos-version-and-hardware-overview-info ()
  "View macOS version and hardware overview info."
  (interactive)
  (dwim-shell-command-on-marked-files
   "macOS hardware overview"
   "sw_vers; system_profiler SPHardwareDataType"
   :utils '("sw_vers" "system_profiler")))

(defun dwim-shell-commands-macos-reveal-in-finder ()
  "Reveal selected files in macOS Finder."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Reveal in Finder"
   "import AppKit
    NSWorkspace.shared.activateFileViewerSelecting([\"<<*>>\"].map{URL(fileURLWithPath:$0)})"
   :silent-success t
   :shell-pipe "swift -"
   :join-separator ", "
   :utils "swift"))

(defun dwim-shell-commands--macos-sharing-services ()
  "Return a list of sharing services."
  (let* ((source (format "import AppKit
                         NSSharingService.sharingServices(forItems: [
                           %s
                         ]).forEach {
                           print(\"\\($0.title)\")
                         }"
                         (string-join (mapcar (lambda (file)
                                                (format "URL(fileURLWithPath: \"%s\")" file))
                                              (dwim-shell-command--files))
                                      ", ")))
         (services (split-string (string-trim (shell-command-to-string (format "echo '%s' | swift -" source)))
                                 "\n")))
    (when (seq-empty-p services)
      (error "No sharing services available"))
    services))

(defun dwim-shell-commands-macos-share ()
  "Share selected files from macOS."
  (interactive)
  (let* ((services (dwim-shell-commands--macos-sharing-services))
         (service-name (completing-read "Share via: " services))
         (selection (seq-position services service-name #'string-equal)))
    (dwim-shell-command-on-marked-files
     "Share"
     (format
      "import AppKit

       _ = NSApplication.shared

       NSApp.setActivationPolicy(.regular)

       class MyWindow: NSWindow, NSSharingServiceDelegate {
         func sharingService(
           _ sharingService: NSSharingService,
           didShareItems items: [Any]
         ) {
           NSApplication.shared.terminate(nil)
         }

         func sharingService(
           _ sharingService: NSSharingService, didFailToShareItems items: [Any], error: Error
         ) {
           let error = error as NSError
           if error.domain == NSCocoaErrorDomain && error.code == NSUserCancelledError {
             NSApplication.shared.terminate(nil)
           }
           exit(1)
         }
       }

       let window = MyWindow(
         contentRect: NSRect(x: 0, y: 0, width: 0, height: 0),
         styleMask: [],
         backing: .buffered,
         defer: false)

       let services = NSSharingService.sharingServices(forItems: [\"<<*>>\"].map{URL(fileURLWithPath:$0)})
       let service = services[%s]
       service.delegate = window
       service.perform(withItems: [\"<<*>>\"].map{URL(fileURLWithPath:$0)})

       NSApp.run()" selection)
     :silent-success t
     :shell-pipe "swift -"
     :join-separator ", "
     :no-progress t
     :utils "swift")))

(defun dwim-shell-commands-macos-toggle-display-rotation ()
  "Rotate display."
  (interactive)
  ;; #  Display_ID    Resolution  ____Display_Bounds____  Rotation
  ;; 2  0x2b347692    1440x2560      0     0  1440  2560    270    [main]
  ;; From fb-rotate output, get the `current-rotation' from Column 7, row 1 zero-based.
  (let ((current-rotation (nth 7 (split-string (nth 1 (process-lines "fb-rotate" "-i"))))))
    (dwim-shell-command-on-marked-files
     "macOS hardware overview"
     (format "fb-rotate -d 1 -r %s" (if (equal current-rotation "270") "0" "270"))
     :utils "fb-rotate")))

(defun dwim-shell-commands--macos-apps ()
  "Return alist of macOS apps (\"Emacs\" . \"/Applications/Emacs.app\")."
  (mapcar (lambda (path)
            (cons (file-name-base path) path))
          (seq-sort
           #'string-lessp
           (seq-mapcat (lambda (paths)
                         (directory-files-recursively
                          paths "\\.app$" t (lambda (path)
                                              (not (string-suffix-p ".app" path)))))
                       '("/Applications" "~/Applications" "/System/Applications")))))

(defun dwim-shell-commands-macos-set-default-app ()
  "Set default app for file(s)."
  (interactive)
  (let* ((apps (dwim-shell-commands--macos-apps))
         (selection (progn
                      (cl-assert apps nil "No apps found")
                      (completing-read "Set default app: " apps nil t))))
    (dwim-shell-command-on-marked-files
     "Set default app"
     (format "duti -s \"%s\" '<<e>>' all"
             (string-trim
              (shell-command-to-string (format "defaults read '%s/Contents/Info.plist' CFBundleIdentifier"
                                               (map-elt apps selection)))))
     :silent-success t
     :no-progress t
     :utils "duti")))

(defun dwim-shell-commands-macos-open-with ()
  "Open file(s) with specific external app."
  (interactive)
  (let* ((apps (dwim-shell-commands--macos-apps))
         (selection (progn
                      (cl-assert apps nil "No apps found")
                      (completing-read "Open with: " apps nil t))))
    (dwim-shell-command-on-marked-files
     "Open with"
     (format "open -a '%s' '<<*>>'" (map-elt apps selection))
     :silent-success t
     :no-progress t
     :utils "open")))

(defun dwim-shell-commands-macos-open-with-firefox ()
  "Open file(s) with specific external app."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Open with Firefox"
   "open -a Firefox '<<*>>'"
   :silent-success t
   :no-progress t
   :utils "open"))

(defun dwim-shell-commands-macos-open-with-safari ()
  "Open file(s) with specific external app."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Open with Firefox"
   "open -a Safari '<<*>>'"
   :silent-success t
   :no-progress t
   :utils "open"))

(defun dwim-shell-commands-macos-start-recording-window ()
  "Select and start recording a macOS window."
  (interactive)
  (let* ((window (dwim-shell-commands--macos-select-window))
         (path (dwim-shell-commands--generate-path "~/Desktop" (car window) ".gif"))
         (buffer-file-name path) ;; override so <<f>> picks it up
         (inhibit-message t))
    ;; Silence echo to avoid unrelated messages making into animation.
    (cl-letf (((symbol-function 'dwim-shell-command--message)
               (lambda (fmt &rest args) nil)))
      (dwim-shell-command-on-marked-files
       "Start recording a macOS window."
       (format
        "macosrec --record '%s' --gif --output '<<f>>'"
        (cdr window))
       :silent-success t
       :monitor-directory "~/Desktop"
       :no-progress t
       :utils '("ffmpeg" "macosrec")
       :on-completion
       (lambda (buffer process)
         (if (= (process-exit-status process) 0)
             (progn
               "Saved recording"
               (dired-jump nil path)
               (kill-buffer buffer))
           (with-current-buffer buffer
             (goto-char (point-min))
             (if (search-forward "Aborted" nil t)
                 (progn
                   (message "Aborted recording")
                   (kill-buffer buffer))
               (switch-to-buffer buffer)))))))))

(defun dwim-shell-commands--generate-path (dir name ext)
  "Generate a timestamped path with DIR, NAME, and EXT."
  (concat (file-name-as-directory (expand-file-name dir))
          (format-time-string "%Y-%m-%d-%H:%M:%S-")
          name ext))

(defun dwim-shell-commands--macos-select-window ()
  "Return a list of macOS windows."
  (if-let* ((line (completing-read
                   "Select: "
                   (process-lines "macosrec" "--list") nil t))
            (window-info (split-string line " "))
            (window-number (string-to-number (nth 0 window-info)))
            (window-app (nth 1 window-info))
            (valid (> window-number 0)))
      (cons window-app window-number)
    (user-error "No window found")))

(defun dwim-shell-commands-macos-end-recording-window ()
  "Stop recording a macOS window."
  (interactive)
  (let ((inhibit-message t))
    (cl-letf (((symbol-function 'dwim-shell-command--message)
               (lambda (fmt &rest args) nil)))
      (dwim-shell-command-on-marked-files
       "End recording macOS window."
       "macosrec --save"
       :silent-success t
       :no-progress t
       :error-autofocus t
       :utils "macosrec"))))

(defun dwim-shell-commands-macos-abort-recording-window ()
  "Stop recording a macOS window."
  (interactive)
  (let ((inhibit-message t))
    (cl-letf (((symbol-function 'dwim-shell-command--message)
               (lambda (fmt &rest args) nil)))
      (dwim-shell-command-on-marked-files
       "Abort recording macOS window."
       "macosrec --abort"
       :silent-success t
       :no-progress t
       :utils "macosrec"))))

(defun dwim-shell-commands-macos-screenshot-window ()
  "Select and screenshot macOS window."
  (interactive)
  ;; Silence echo to avoid unrelated messages making into screenshot.
  (let ((window (dwim-shell-commands--macos-select-window))
        (inhibit-message t))
    (dwim-shell-command-on-marked-files
     "Start recording a macOS window."
     (format "macosrec --screenshot %s" (cdr window))
     :silent-success t
     :monitor-directory "~/Desktop"
     :no-progress t
     :utils "macosrec")))

(defun dwim-shell-commands-files-combined-size ()
  "Get files combined file size."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Get files combined file size"
   "du -csh '<<*>>'"
   :utils "du"
   :on-completion (lambda (buffer _process)
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
       :on-completion (lambda (buffer _process)
                        (kill-buffer buffer)
                        (dired project-dir))))))

(defun dwim-shell-commands-http-serve-dir ()
  "HTTP serve current directory."
  (interactive)
  (cond ((executable-find "python3")
         (dwim-shell-command-on-marked-files
          "HTTP serve current dir"
          "python3 -m http.server"
          :utils "python3"
          :focus-now t
          :no-progress t))
        ((executable-find "python2")
         (dwim-shell-command-on-marked-files
          "HTTP serve current dir"
          "python2 -m SimpleHTTPServer"
          :utils "python2"
          :focus-now t
          :no-progress t))
        ((executable-find "python")
         (dwim-shell-command-on-marked-files
          "HTTP serve current dir"
          "python -m SimpleHTTPServer"
          :utils "python"
          :focus-now t
          :no-progress t))
        (t
         (error "No python found"))))

(defun dwim-shell-commands-git-clone-clipboard-url ()
  "Clone git URL in clipboard to `default-directory'."
  (interactive)
  (dwim-shell-command-on-marked-files
   (format "Clone %s" (file-name-base (current-kill 0)))
   "git clone <<cb>>"
   :utils "git"))

(defun dwim-shell-commands-pass-git-pull ()
  "Pass git pull."
  (interactive)
  (dwim-shell-command-on-marked-files
   "pass git pull"
   "pass git pull"
   :utils '("pass" "git")
   :silent-success t))

(defun dwim-shell-commands-git-list-untracked-files ()
  "List untracked git files in `default-directory'."
  (interactive)
  (dwim-shell-command-on-marked-files
   "List untracked"
   "git ls-files --others ."
   :utils "git"
   :focus-now t))

(defun dwim-shell-commands-git-delete-untracked-files ()
  "Delete untracked git files in `default-directory'."
  (interactive)
  (when (y-or-n-p (format "Clean '%s'? \n\n%s\n...\n\n"
                          default-directory
                          (string-join
                           (seq-take (process-lines "git" "ls-files" "--others" ".") 3)
                           "\n")))
    (dwim-shell-command-on-marked-files
     "Clean untracked"
     "git clean -f ."
     :utils "git"
     :silent-success t)))

(defun dwim-shell-commands-external-ip ()
  "Copy external IP to kill ring."
  (interactive)
  (let ((ip (car (last (process-lines "curl" "ifconfig.me")))))
    (kill-new ip)
    (message "Copied %s" ip)))

(defun dwim-shell-commands-macos-install-iphone-device-ipa ()
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

(defun dwim-shell-commands-duplicate ()
  "Duplicate file."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Duplicate file(s)."
   "cp -R '<<f>>' '<<f(u)>>'"
   :utils "cp"))

(defun dwim-shell-commands-rename-all ()
  "Rename all marked file(s)."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Rename all"
   "mv '<<f>>' '<<New name:Renamed>>(<<n>>).<<e>>'"
   :utils "mv"))

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

(defun dwim-shell-commands-kill-gpg-agent ()
  "Kill (thus restart) gpg agent.

Useful for when you get this error:

gpg: public key decryption failed: No pinentry
gpg: decryption failed: No pinentry"
  (interactive)
  (dwim-shell-command-on-marked-files
   "Kill gpg agent"
   "gpgconf --kill gpg-agent"
   :utils "gpgconf"
   :silent-success t))

;; Based on
;; https://apps.bram85.nl/git/bram/gists/src/commit/31ac3363da925daafa2420b7f96c67612ca28241/gists/dwim-0x0-upload.el
(defun dwim-shell-commands-upload-to-0x0 ()
  "Upload the marked files to 0x0.st"
  (interactive)
  (dwim-shell-command-on-marked-files
   "0x0 upload"
   "curl -Ffile=@<<f>> -Fsecret= https://0x0.st"
   :utils "curl"
   :post-process-template
   ;; Insert the single quotes at the appropriate place according to
   ;; 0x0.st example online:
   ;; curl -F'file=@yourfile.png' -Fsecret= https://0x0.st
   ;;
   ;; The placement of these single quotes confuse the escaping
   ;; mechanisms of dwim-shell-command, as it considers @ as the
   ;; opening 'quote' as it appears right in front of <<f>>.
   (lambda (template path)
     (string-replace "-Ffile" "-F'file"
                     (string-replace path (concat path "'") template)))
   :on-completion
   (lambda (buffer process)
     (if (= (process-exit-status process) 0)
         (with-current-buffer buffer
           (let ((url (car (last (split-string (string-trim (buffer-string)) "\n")))))
             (eww url)
             (kill-new url)
             (message "Copied: %s" (current-kill 0)))
           (kill-buffer buffer))
       (switch-to-buffer buffer)))))

(provide 'dwim-shell-commands)

;;; dwim-shell-commands.el ends here
