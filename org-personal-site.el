;;; org-personal-site.el --- Static site generator for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nana Adane

;; Author: Nana Adane
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5.1"))
;; Keywords: org, blog, static-site, publishing
;; URL: https://github.com/yourusername/org-personal-site

;;; Commentary:

;; A frictionless static site generator for org-mode that prioritizes
;; ease of use and minimal configuration.  This package provides a
;; complete workflow for creating, previewing, and deploying static
;; websites from org-mode files.
;;
;; Key features:
;; - Emacs-native implementation (no external scripts)
;; - Live preview with automatic rebuilding
;; - Built-in theme system with full customization
;; - Seamless Magit integration for deployment
;; - RSS feed generation
;; - Image insertion helpers
;;
;; Quick start:
;;
;; 1. Install and configure:
;;
;;    (use-package org-personal-site
;;      :custom
;;      (org-personal-site-source-dir "~/org/my-blog")
;;      (org-personal-site-output-dir "~/my-site.github.io")
;;      (org-personal-site-name "My Site")
;;      (org-personal-site-author "Your Name"))
;;
;; 2. Create directory structure:
;;    - source-dir/content/index.org (homepage)
;;    - source-dir/content/blog/ (blog posts)
;;    - source-dir/content/media/ (images)
;;
;; 3. Use commands:
;;    - M-x org-personal-site-new-post - Create new blog post
;;    - M-x org-personal-site-preview - Build and preview with live reload
;;    - M-x org-personal-site-deploy - Build and deploy via git
;;
;; Optional dependencies:
;;    - htmlize (for syntax highlighting in code blocks)
;;      Install with: M-x package-install RET htmlize RET
;;
;; See README.md for comprehensive documentation.

;;; Code:

(require 'ox-html)
(require 'ox-publish)
(require 'simple-httpd)
(require 'filenotify)

;; Optional: htmlize for syntax highlighting
(when (require 'htmlize nil 'noerror)
  (setq org-html-htmlize-output-type 'css))

;;; Configuration

(defgroup org-personal-site nil
  "Static site generator for org-mode."
  :group 'org
  :prefix "org-personal-site-")

(defcustom org-personal-site-source-dir "~/org/site"
  "Directory containing org source files.
Should contain:
  - content/index.org (homepage)
  - content/blog/ (blog posts)
  - content/media/ (images, optional)"
  :type 'directory
  :group 'org-personal-site)

(defcustom org-personal-site-output-dir "~/site-output"
  "Directory for generated HTML files.
This should be a git repository for deployment to GitHub Pages
or other static hosting services."
  :type 'directory
  :group 'org-personal-site)

(defcustom org-personal-site-name "My Site"
  "Name of the site.
Appears in the header and site metadata."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-author "Your Name"
  "Author name for site metadata and copyright."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-url ""
  "Base URL of the deployed site (e.g., https://example.com).
Used for RSS feed and canonical URLs."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-cname ""
  "Custom domain for GitHub Pages (e.g., example.com).
When set, a CNAME file will be generated in the output directory.
Leave empty if not using a custom domain."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-description ""
  "Site description for RSS feed and metadata."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-theme 'opencode-tui
  "Theme preset or custom theme configuration.
Can be a symbol for a built-in theme:
  - opencode-tui (dark, warm, terminal-inspired)
  - light (light background, dark text)
  - gruvbox (Gruvbox dark theme)

Or a plist for custom theme:
  (:background \"#131010\"
   :foreground \"#d4a574\"
   :accent \"#d4a574\")"
  :type '(choice (const :tag "Opencode TUI" opencode-tui)
                 (const :tag "Light" light)
                 (const :tag "Gruvbox" gruvbox)
                 (plist :tag "Custom theme"
                        :options ((:background string)
                                  (:foreground string)
                                  (:accent string))))
  :group 'org-personal-site)

(defcustom org-personal-site-font-logo "Pixelify Sans"
  "Font for site logo/name."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-font-heading "JetBrains Mono"
  "Font for headings and code."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-font-body "Iosevka Etoile"
  "Font for body text."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-font-body-cdn nil
  "Custom CDN URL for body font (if not on Google Fonts).
When non-nil, this URL is used instead of Google Fonts CDN for
the body font.  Useful for fonts like Iosevka from cdnjs.

Example:
  \"https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css\""
  :type '(choice (const :tag "Use Google Fonts" nil)
                 (string :tag "Custom CDN URL"))
  :group 'org-personal-site)

(defcustom org-personal-site-custom-head-html ""
  "Additional HTML to inject in <head> section.
Use this for custom fonts, analytics, meta tags, etc."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-server-port 8080
  "Port for preview HTTP server."
  :type 'integer
  :group 'org-personal-site)

(defcustom org-personal-site-use-python-server t
  "Whether to use Python's http.server for preview instead of simple-httpd.
Python's http.server is faster and doesn't slow down Emacs, but requires
Python to be installed. When nil, uses simple-httpd (pure Elisp)."
  :type 'boolean
  :group 'org-personal-site)

(defcustom org-personal-site-auto-push t
  "Whether to prompt for push confirmation when deploying.
When nil, stop after commit and let user push manually via Magit."
  :type 'boolean
  :group 'org-personal-site)

(defcustom org-personal-site-favicon-letter nil
  "Letter to use in favicon (single character).
When nil, uses first letter of `org-personal-site-name'."
  :type '(choice (const :tag "Auto (first letter of site name)" nil)
                 (string :tag "Custom letter"))
  :group 'org-personal-site)

(defcustom org-personal-site-favicon-font "Pixelify Sans"
  "Font to use for favicon letter."
  :type 'string
  :group 'org-personal-site)

(defcustom org-personal-site-recent-posts-count 5
  "Number of recent posts to show on homepage."
  :type 'integer
  :group 'org-personal-site)

;;; Theme Presets

(defconst org-personal-site--themes
  '((opencode-tui
     :background "#131010"
     :foreground "#d4a574"
     :accent "#d4a574"
     :description "Dark, warm, terminal-inspired theme")
    (light
     :background "#ffffff"
     :foreground "#333333"
     :accent "#0066cc"
     :description "Clean light theme")
    (gruvbox
     :background "#282828"
     :foreground "#ebdbb2"
     :accent "#fe8019"
     :description "Gruvbox dark theme"))
  "Built-in theme presets.")

;;; Internal Variables

(defvar org-personal-site--preview-process nil
  "HTTP server process for preview.")

(defvar org-personal-site--preview-watchers nil
  "List of file watcher descriptors for preview.")

(defvar org-personal-site--rebuild-timer nil
  "Timer for debounced rebuilds during preview.")

(defvar org-personal-site--last-rebuild-time nil
  "Time of last rebuild to prevent loops.")

(defvar org-personal-site--rebuilding nil
  "Flag to prevent concurrent rebuilds.")

;;; Utility Functions

(defun org-personal-site--message (format-string &rest args)
  "Display a message with org-personal-site prefix.
FORMAT-STRING and ARGS are passed to `message'."
  (apply #'message (concat "[org-personal-site] " format-string) args))

(defun org-personal-site--get-theme ()
  "Get the current theme configuration as a plist.
If `org-personal-site-theme' is a symbol, look up the preset.
Otherwise, return it as-is."
  (if (symbolp org-personal-site-theme)
      (or (cdr (assq org-personal-site-theme org-personal-site--themes))
          (error "Unknown theme: %s" org-personal-site-theme))
    org-personal-site-theme))

(defun org-personal-site--slugify (title)
  "Convert TITLE to URL-safe slug.
Converts to lowercase, replaces spaces with hyphens,
removes special characters, and ensures the result is
suitable for use in filenames and URLs.

Examples:
  (org-personal-site--slugify \"My First Post\")
  => \"my-first-post\"
  
  (org-personal-site--slugify \"Hello, World!\")
  => \"hello-world\""
  (let ((slug (downcase title)))
    ;; Replace spaces with hyphens
    (setq slug (replace-regexp-in-string " " "-" slug))
    ;; Remove special characters except hyphens
    (setq slug (replace-regexp-in-string "[^a-z0-9-]" "" slug))
    ;; Remove multiple consecutive hyphens
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    ;; Remove leading/trailing hyphens
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    slug))

(defun org-personal-site--get-google-fonts-url ()
  "Generate Google Fonts URL for configured fonts.
Returns a URL that includes logo, heading, and optionally body font."
  (let* ((fonts (list org-personal-site-font-logo
                     org-personal-site-font-heading))
         ;; Only add body font if no custom CDN is specified
         (fonts (if org-personal-site-font-body-cdn
                   fonts
                 (cons org-personal-site-font-body fonts)))
         (fonts (delete-dups fonts))
         (encoded-fonts (mapconcat (lambda (f)
                                    (replace-regexp-in-string " " "+" f))
                                  fonts "&family=")))
    (format "https://fonts.googleapis.com/css2?family=%s&display=swap"
            encoded-fonts)))

;;; Directory Validation

(defun org-personal-site--validate-structure ()
  "Check that source directory has expected structure.
Returns nil if valid, error message string if invalid."
  (let ((source-dir (expand-file-name org-personal-site-source-dir))
        (content-dir (expand-file-name "content" org-personal-site-source-dir))
        (blog-dir (expand-file-name "content/blog" org-personal-site-source-dir))
        (index-file (expand-file-name "content/index.org" org-personal-site-source-dir)))
    (cond
     ((not (file-directory-p source-dir))
      (format "Source directory does not exist: %s" source-dir))
     ((not (file-directory-p content-dir))
      (format "Missing required directory: %s/content/\n\nHint: Create content/ directory in your source dir." 
              source-dir))
     ((not (file-directory-p blog-dir))
      (format "Missing required directory: %s/content/blog/\n\nHint: Create content/blog/ for blog posts." 
              source-dir))
     ((not (file-exists-p index-file))
      (format "Missing required file: %s/content/index.org\n\nHint: Create an index.org file with site homepage content." 
              source-dir))
     (t nil))))

(defun org-personal-site--ensure-valid ()
  "Ensure site structure is valid, error if not."
  (when-let ((error-msg (org-personal-site--validate-structure)))
    (user-error "Invalid site structure:\n\n%s" error-msg)))

;;; Template Generation

(defun org-personal-site--generate-css-content ()
  "Generate CSS content from theme configuration."
  (let* ((theme (org-personal-site--get-theme))
         (bg (plist-get theme :background))
         (fg (plist-get theme :foreground))
         (accent (plist-get theme :accent)))
    (format "/* Opencode TUI Theme - Generated by org-personal-site.el */
:root {
  --bg: %s;
  --fg: #eee;
  --card-bg: #1E1A1A;
  --border: #3D3838;
  --link: %s;
  --link-hover: #e8b889;
  --dim: #888;
  --code-bg: #1E1A1A;
  --font-main: '%s', 'Iosevka', serif;
  --font-mono: '%s', ui-monospace, 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', monospace;
}

body {
  background-color: var(--bg);
  color: var(--fg);
  font-family: var(--font-main);
  font-size: 19px;
  line-height: 1.6;
  max-width: 720px;
  margin: 0 auto;
  padding: 2rem 1rem;
}

/* Typography */
h1, h2, h3, h4 { font-family: var(--font-mono); font-weight: 700; margin-top: 2.5rem; color: #fff; }
h1 { font-size: 2rem; border-bottom: 2px solid var(--border); padding-bottom: 0.5rem; margin-top: 0; }
a { color: var(--link); text-decoration: none; border-bottom: 1px dotted var(--border); transition: 0.2s; }
a:hover { color: var(--link-hover); border-bottom-style: solid; }

/* Header & Nav */
header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 3rem; font-family: var(--font-mono); border-bottom: 1px solid var(--border); padding-bottom: 1.5rem; }
header .logo { 
  font-family: '%s', monospace;
  font-weight: 600; 
  font-size: 2rem; 
  color: var(--link); 
  border: none; 
  letter-spacing: 0.05em;
  text-shadow: 3px 3px 0 var(--border);
  transition: all 0.2s;
  line-height: 1.4;
  padding: 0.25rem 0;
  text-transform: lowercase;
}
header .logo:hover { 
  color: var(--link-hover); 
  text-shadow: 4px 4px 0 var(--border);
  transform: translateY(-2px);
}
nav ul { list-style: none; display: flex; gap: 1.5rem; margin: 0; padding: 0; }
nav a { color: var(--dim); border: none; }
nav a:hover { color: var(--fg); }

/* Content */
.content img { max-width: 100%%; border-radius: 4px; display: block; margin: 1.5rem auto; }
code { font-family: var(--font-mono); background: var(--code-bg); padding: 0.2rem 0.4rem; border-radius: 3px; font-size: 0.85em; }
pre { background: var(--code-bg); padding: 1rem; overflow-x: auto; border-radius: 4px; border: 1px solid #333; }
pre code { background: none; padding: 0; color: #ccc; }
blockquote { border-left: 4px solid var(--link); margin: 1.5rem 0; padding-left: 1rem; color: var(--dim); font-style: italic; }
p { margin: 1rem 0; }
ul, ol { margin: 1rem 0; padding-left: 2rem; }
li { margin: 0.5rem 0; }
img { max-width: 100%%; height: auto; display: block; margin: 1.5rem auto; border-radius: 4px; }
.figure { margin: 2rem 0; }
.figure p { text-align: center; font-style: italic; color: var(--dim); margin-top: 0.5rem; }

/* Blog Index */
.archive-item { display: flex; justify-content: flex-start; gap: 2rem; margin-bottom: 0.8rem; font-family: var(--font-mono); align-items: baseline; }
.archive-date { color: var(--dim); font-size: 0.85em; min-width: 110px; font-variant-numeric: tabular-nums; }

/* Footer */
footer { margin-top: 5rem; padding-top: 2rem; border-top: 1px solid var(--border); font-size: 0.9rem; color: var(--dim); text-align: center; font-family: var(--font-mono); }
"
            bg accent
            org-personal-site-font-body
            org-personal-site-font-heading
            org-personal-site-font-logo)))

(defun org-personal-site--generate-favicon-svg ()
  "Generate favicon SVG content."
  (let* ((letter (or org-personal-site-favicon-letter
                    (substring org-personal-site-name 0 1)))
         (theme (org-personal-site--get-theme))
         (bg (plist-get theme :background))
         (fg (plist-get theme :accent)))
    (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 100 100\">
  <rect width=\"100\" height=\"100\" fill=\"%s\"/>
  <text x=\"50\" y=\"50\" font-family=\"%s\" font-size=\"60\" 
        fill=\"%s\" text-anchor=\"middle\" dominant-baseline=\"central\">%s</text>
</svg>"
            bg
            org-personal-site-favicon-font
            fg
            letter)))

(defun org-personal-site--generate-html-preamble (info)
  "Generate HTML preamble for pages.
INFO is the plist from org-publish."
  (let* ((theme (org-personal-site--get-theme))
         (google-fonts-url (org-personal-site--get-google-fonts-url))
         (body-font-html (if org-personal-site-font-body-cdn
                            (format "<link rel=\"stylesheet\" href=\"%s\"/>"
                                   org-personal-site-font-body-cdn)
                          ""))
         (custom-head (if (string-empty-p org-personal-site-custom-head-html)
                         ""
                       org-personal-site-custom-head-html)))
    (format "<link rel=\"stylesheet\" href=\"/links/style.css\"/>
                     <link rel=\"stylesheet\" href=\"%s\"/>
                     %s
                     <link rel=\"icon\" href=\"/favicon.svg\" type=\"image/svg+xml\"/>
                     %s
</head>
<body>
<div id=\"preamble\" class=\"status\">
<header>
  <a href=\"/\" class=\"logo\">%s</a>
  <nav>
    <ul>
      <li><a href=\"/index.html\">bio</a></li>
      <li><a href=\"/blog/index.html\">blog</a></li>
      <li><a href=\"/projects.html\">projects</a></li>
    </ul>
  </nav>
</header>
<main class=\"content\">"
            google-fonts-url
            body-font-html
            custom-head
            org-personal-site-name)))

(defun org-personal-site--generate-html-postamble (info)
  "Generate HTML postamble for pages.
INFO is the plist from org-publish."
  (format "</main>
<footer>
  <p>© %s %s | <a href=\"/feed.xml\">RSS</a></p>
</footer>"
          (format-time-string "%Y")
          org-personal-site-author))

;;; Blog Post Collection

(defun org-personal-site--collect-blog-posts ()
  "Collect all blog posts with metadata.
Returns a list of plists with :date, :title, :file, :link properties."
  (let* ((blog-dir (expand-file-name "content/blog" org-personal-site-source-dir))
         (blog-files (directory-files blog-dir t "\\.org$"))
         (posts '()))
    
    (dolist (file blog-files)
      (unless (string-match-p "\\(index\\|rss\\|feed\\)\\.org$" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let* ((title "Untitled")
                 (date (current-time))
                 (tags nil)
                 (draft nil))
            
            ;; Extract title (case-insensitive)
            (when (re-search-forward "^#\\+[Tt][Ii][Tt][Ll][Ee]: \\(.+\\)$" nil t)
              (setq title (match-string 1)))
            
            ;; Extract date (handle both <> and [] formats, case-insensitive)
            (goto-char (point-min))
            (when (or (re-search-forward "^#\\+[Dd][Aa][Tt][Ee]: <\\([^>]+\\)>" nil t)
                     (re-search-forward "^#\\+[Dd][Aa][Tt][Ee]: \\[\\([^]]+\\)\\]" nil t))
              (condition-case nil
                  (setq date (apply 'encode-time (org-parse-time-string (match-string 1))))
                (error nil)))
            
            ;; Extract tags
            (goto-char (point-min))
            (when (re-search-forward "^#\\+[Tt][Aa][Gg][Ss]: \\(.+\\)$" nil t)
              (setq tags (split-string (match-string 1) "," t " ")))
            
            ;; Check for draft status
            (goto-char (point-min))
            (when (re-search-forward "^#\\+[Dd][Rr][Aa][Ff][Tt]: \\(t\\|true\\|yes\\)" nil t)
              (setq draft t))
            
            ;; Only include non-drafts
            (unless draft
              (push (list :date date
                         :title title
                         :file file
                         :link (concat "blog/" 
                                      (file-name-sans-extension 
                                       (file-name-nondirectory file))
                                      ".html")
                         :tags tags)
                    posts))))))
    
    ;; Sort by date (newest first)
    (sort posts (lambda (a b) 
                  (time-less-p (plist-get b :date) 
                              (plist-get a :date))))))

(defun org-personal-site--generate-recent-posts-html ()
  "Generate HTML for recent posts section.
Returns HTML string to be injected into the index page."
  (let* ((posts (org-personal-site--collect-blog-posts))
         (recent (seq-take posts org-personal-site-recent-posts-count))
         (html ""))
    (setq html (concat html "<ul>\n"))
    (dolist (post recent)
      (setq html (concat html
                        (format "<li>%s - <a href=\"%s\">%s</a></li>\n"
                               (format-time-string "%Y-%m-%d" (plist-get post :date))
                               (plist-get post :link)
                               (plist-get post :title)))))
    (setq html (concat html "</ul>\n"))
    (setq html (concat html "<p><a href=\"blog/index.html\">View all posts →</a></p>\n"))
    html))

(defun org-personal-site--postprocess-index ()
  "Post-process index.html to inject recent posts.
This is called after org-publish has generated the HTML."
  (let* ((index-file (expand-file-name "index.html" org-personal-site-output-dir))
         (recent-html (org-personal-site--generate-recent-posts-html)))
    
    (when (file-exists-p index-file)
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-min))
        
        ;; Find the Recent Writing section and inject HTML
        (when (re-search-forward "<h2[^>]*>Recent Writing</h2>" nil t)
          (forward-line 1)
          ;; Delete any existing list until next heading or end of content
          (let ((start (point)))
            (if (re-search-forward "<h[12]" nil t)
                (beginning-of-line)
              (re-search-forward "</main>\\|<footer>" nil t)
              (beginning-of-line))
            (delete-region start (point)))
          
          ;; Insert recent posts HTML
          (insert recent-html))
        
        ;; Write back
        (write-region (point-min) (point-max) index-file nil 'silent)))))

;;; RSS Feed Generation

(defun org-personal-site--generate-rss ()
  "Generate RSS feed from blog posts."
  (let* ((posts (org-personal-site--collect-blog-posts))
         (rss-file (expand-file-name "feed.xml" org-personal-site-output-dir))
         (site-url (if (string-empty-p org-personal-site-url)
                      "https://example.com"
                    org-personal-site-url))
         (site-desc (if (string-empty-p org-personal-site-description)
                       (format "Blog posts from %s" org-personal-site-name)
                     org-personal-site-description)))
    
    (with-temp-buffer
      ;; RSS header
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
      (insert "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n")
      (insert "  <channel>\n")
      (insert (format "    <title>%s</title>\n" org-personal-site-name))
      (insert (format "    <link>%s</link>\n" site-url))
      (insert (format "    <description>%s</description>\n" site-desc))
      (insert (format "    <language>en</language>\n"))
      (insert (format "    <lastBuildDate>%s</lastBuildDate>\n" 
                     (format-time-string "%a, %d %b %Y %H:%M:%S %z")))
      (insert (format "    <atom:link href=\"%s/feed.xml\" rel=\"self\" type=\"application/rss+xml\" />\n" 
                     site-url))
      
      ;; RSS items (limit to 20)
      (dolist (post (seq-take posts 20))
        (let* ((title (plist-get post :title))
               (link (concat site-url "/" (plist-get post :link)))
               (date (plist-get post :date))
               (pub-date (format-time-string "%a, %d %b %Y %H:%M:%S %z" date))
               ;; Extract description from file
               (description (org-personal-site--extract-description 
                            (plist-get post :file))))
          
          (insert "    <item>\n")
          (insert (format "      <title>%s</title>\n" 
                         (org-personal-site--escape-xml title)))
          (insert (format "      <link>%s</link>\n" link))
          (insert (format "      <guid>%s</guid>\n" link))
          (insert (format "      <pubDate>%s</pubDate>\n" pub-date))
          (insert (format "      <description>%s</description>\n" 
                         (org-personal-site--escape-xml description)))
          (insert "    </item>\n")))
      
      ;; RSS footer
      (insert "  </channel>\n")
      (insert "</rss>\n")
      
      ;; Write to file
      (write-region (point-min) (point-max) rss-file nil 'silent))
    
    (org-personal-site--message "RSS feed generated: %s" rss-file)))

(defun org-personal-site--extract-description (file)
  "Extract first paragraph from org FILE for RSS description."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    
    ;; Skip metadata
    (while (looking-at "^#\\+")
      (forward-line 1))
    
    ;; Skip headings and get first paragraph
    (while (looking-at "^\\*\\|^[ \t]*$")
      (forward-line 1))
    
    (let ((start (point)))
      (if (re-search-forward "^[ \t]*$\\|^\\*" nil t)
          (buffer-substring-no-properties start (match-beginning 0))
        (buffer-substring-no-properties start (point-max))))))

(defun org-personal-site--escape-xml (text)
  "Escape TEXT for XML/RSS feed."
  (let ((text (replace-regexp-in-string "&" "&amp;" text)))
    (setq text (replace-regexp-in-string "<" "&lt;" text))
    (setq text (replace-regexp-in-string ">" "&gt;" text))
    (setq text (replace-regexp-in-string "\"" "&quot;" text))
    (setq text (replace-regexp-in-string "'" "&apos;" text))
    text))

;;; Build Configuration

(defun org-personal-site--cleanup-orphaned-files ()
  "Remove HTML files in output directory that no longer have source org files."
  (let* ((blog-source (expand-file-name "content/blog" org-personal-site-source-dir))
         (blog-output (expand-file-name "blog" org-personal-site-output-dir))
         (source-files (when (file-directory-p blog-source)
                        (directory-files blog-source nil "\\.org$")))
         (output-files (when (file-directory-p blog-output)
                        (directory-files blog-output nil "\\.html$")))
         (removed-count 0))
    
    ;; Convert source org files to expected html filenames
    (let ((expected-html (mapcar (lambda (f)
                                  (concat (file-name-sans-extension f) ".html"))
                                source-files)))
      
      ;; Remove HTML files that don't have corresponding org source
      (dolist (html-file output-files)
        (unless (or (member html-file expected-html)
                   (string= html-file "index.html"))  ; Keep blog index
          (let ((file-path (expand-file-name html-file blog-output)))
            (delete-file file-path)
            (setq removed-count (1+ removed-count))
            (org-personal-site--message "Removed orphaned file: %s" html-file)))))
    
    (when (> removed-count 0)
      (org-personal-site--message "Cleaned up %d orphaned file(s)" removed-count))))

(defun org-personal-site--build-publish-config ()
  "Build org-publish-project-alist dynamically from configuration."
  (let ((source-dir (expand-file-name org-personal-site-source-dir))
        (output-dir (expand-file-name org-personal-site-output-dir)))
    
    (list
     ;; Pages (root content)
     (list "pages"
           :base-directory (expand-file-name "content" source-dir)
           :base-extension "org"
           :publishing-directory output-dir
           :publishing-function 'org-html-publish-to-html
           :exclude "blog/.*"
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble #'org-personal-site--generate-html-preamble
           :html-postamble #'org-personal-site--generate-html-postamble
           :with-toc nil
           :section-numbers nil
           :time-stamp-file nil)
     
     ;; Blog posts
     (list "blog"
           :base-directory (expand-file-name "content/blog" source-dir)
           :base-extension "org"
           :publishing-directory (expand-file-name "blog" output-dir)
           :publishing-function 'org-html-publish-to-html
           :exclude "\\(index\\|rss\\|feed\\)\\.org"
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble #'org-personal-site--generate-html-preamble
           :html-postamble #'org-personal-site--generate-html-postamble
           :with-toc nil
           :section-numbers nil
           :time-stamp-file nil
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Blog"
           :sitemap-sort-files 'anti-chronologically
           :sitemap-format-entry #'org-personal-site--sitemap-entry)
     
     ;; Static assets
     (list "assets"
           :base-directory (expand-file-name "content" source-dir)
           :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|pdf"
           :publishing-directory output-dir
           :recursive t
           :publishing-function 'org-publish-attachment)
     
     ;; Combined project
     (list "site" :components '("pages" "blog" "assets")))))

(defun org-personal-site--sitemap-entry (entry style project)
  "Format ENTRY for blog sitemap.
ENTRY is the org file, STYLE is the sitemap style, PROJECT is the current project."
  (when (not (directory-name-p entry))
    (format "[[file:%s][%s]] - %s"
            entry
            (org-publish-find-title entry project)
            (format-time-string "%Y-%m-%d" 
                              (org-publish-find-date entry project)))))

;;; Main Build Command

;;;###autoload
(defun org-personal-site-build ()
  "Build the static site from source to output directory.

This command:
1. Validates the source directory structure
2. Generates CSS from theme configuration
3. Runs org-publish to convert .org files to HTML
4. Generates RSS feed
5. Copies favicon to output directory

The output directory is specified by `org-personal-site-output-dir'
and should be a git repository for deployment.

See also: `org-personal-site-deploy', `org-personal-site-preview'."
  (interactive)
  (org-personal-site--ensure-valid)
  
  (org-personal-site--message "Building site...")
  (let ((start-time (current-time)))
    
    ;; Clean up orphaned files first
    (org-personal-site--cleanup-orphaned-files)
    
    ;; Set up org-publish configuration
    (let ((org-publish-project-alist (org-personal-site--build-publish-config))
          (org-export-with-broken-links 'mark))
      
      ;; Publish all
      (org-publish-all t)
      
      ;; Post-process index.html to inject recent posts
      (org-personal-site--postprocess-index)
      
      ;; Generate CSS
      (let ((css-dir (expand-file-name "links" org-personal-site-output-dir)))
        (make-directory css-dir t)
        (let ((css-file (expand-file-name "style.css" css-dir)))
          (write-region (org-personal-site--generate-css-content)
                       nil css-file nil 'silent)
          (org-personal-site--message "Generated CSS: %s" css-file)))
      
      ;; Generate favicon
      (let ((favicon-file (expand-file-name "favicon.svg" 
                                           org-personal-site-output-dir)))
        (write-region (org-personal-site--generate-favicon-svg)
                     nil favicon-file nil 'silent)
        (org-personal-site--message "Generated favicon: %s" favicon-file))
      
      ;; Generate RSS
      (org-personal-site--generate-rss)
      
      ;; Generate CNAME if custom domain is set
      (when (and org-personal-site-cname 
                 (not (string-empty-p org-personal-site-cname)))
        (let ((cname-file (expand-file-name "CNAME" org-personal-site-output-dir)))
          (write-region org-personal-site-cname nil cname-file nil 'silent)
          (org-personal-site--message "Generated CNAME: %s" cname-file)))
      
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (org-personal-site--message "Build complete in %.2fs! Output: %s" 
                                   elapsed
                                   org-personal-site-output-dir)))))

;;; Preview Mode

;;;###autoload
(defun org-personal-site-preview ()
  "Build site, start HTTP server, and watch for changes.

This command:
1. Builds the site
2. Starts an HTTP server on `org-personal-site-server-port'
3. Opens your browser to preview the site
4. Watches for file changes and rebuilds automatically

The server type is controlled by `org-personal-site-use-python-server'.
Python's http.server is recommended as it's faster and doesn't slow
down Emacs.

Use `org-personal-site-preview-stop' to stop the server and watcher.

See also: `org-personal-site-build', `org-personal-site-preview-stop'."
  (interactive)
  (org-personal-site--ensure-valid)
  
  ;; Stop existing preview if running
  (when org-personal-site--preview-watchers
    (org-personal-site-preview-stop))
  
  ;; Build first
  (org-personal-site-build)
  
  ;; Start HTTP server
  (if org-personal-site-use-python-server
      ;; Use Python's http.server (faster, doesn't slow Emacs)
      (let ((default-directory (expand-file-name org-personal-site-output-dir)))
        (setq org-personal-site--preview-process
              (start-process "org-personal-site-server"
                           "*org-personal-site-server*"
                           "python3" "-m" "http.server"
                           (number-to-string org-personal-site-server-port)))
        (set-process-query-on-exit-flag org-personal-site--preview-process nil)
        (org-personal-site--message "Started Python HTTP server"))
    ;; Use simple-httpd (pure Elisp, but slower)
    (require 'simple-httpd)
    (setq httpd-root org-personal-site-output-dir)
    (setq httpd-port org-personal-site-server-port)
    (httpd-start)
    (setq org-personal-site--preview-process t)
    (org-personal-site--message "Started simple-httpd server"))
  
  ;; Set up file watchers (watch content/ and content/blog/ separately)
  (setq org-personal-site--preview-watchers nil)
  (dolist (dir '("content" "content/blog" "content/media"))
    (let ((watch-dir (expand-file-name dir org-personal-site-source-dir)))
      (when (file-directory-p watch-dir)
        (push (file-notify-add-watch
               watch-dir
               '(change)
               #'org-personal-site--on-file-change)
              org-personal-site--preview-watchers))))
  
  ;; Reset rebuild tracking
  (setq org-personal-site--last-rebuild-time nil
        org-personal-site--rebuilding nil)
  
  ;; Open browser
  (browse-url (format "http://localhost:%d" org-personal-site-server-port))
  
  (org-personal-site--message 
   "Preview started at http://localhost:%d (M-x org-personal-site-preview-stop to stop)"
   org-personal-site-server-port))

;;;###autoload
(defun org-personal-site-preview-stop ()
  "Stop preview server and file watcher.

See also: `org-personal-site-preview'."
  (interactive)
  
  ;; Stop file watchers
  (when org-personal-site--preview-watchers
    (dolist (watcher org-personal-site--preview-watchers)
      (file-notify-rm-watch watcher))
    (setq org-personal-site--preview-watchers nil))
  
  ;; Cancel any pending rebuild and reset flags
  (when org-personal-site--rebuild-timer
    (cancel-timer org-personal-site--rebuild-timer)
    (setq org-personal-site--rebuild-timer nil))
  (setq org-personal-site--last-rebuild-time nil
        org-personal-site--rebuilding nil)
  
  ;; Stop HTTP server
  (when org-personal-site--preview-process
    (if (processp org-personal-site--preview-process)
        ;; Python process
        (progn
          (delete-process org-personal-site--preview-process)
          (org-personal-site--message "Stopped Python HTTP server"))
      ;; simple-httpd
      (httpd-stop)
      (org-personal-site--message "Stopped simple-httpd server"))
    (setq org-personal-site--preview-process nil))
  
  (org-personal-site--message "Preview stopped."))

(defun org-personal-site--should-trigger-rebuild (event)
  "Determine if EVENT should trigger a rebuild.
Filters out temporary files and non-org files."
  (when event
    (let* ((action (nth 1 event))
           (file (nth 2 event))
           (filename (when file (file-name-nondirectory file))))
      (and
       ;; Check action type
       (member action '(changed created deleted renamed))
       ;; Only org files or media files
       (or (string-match-p "\\.org$" filename)
           (string-match-p "\\.[png|jpg|jpeg|gif|svg]$" filename))
       ;; Exclude Emacs temporary/backup files
       (not (string-match-p "~$" filename))           ; Backup files (file.org~)
       (not (string-match-p "^\\.#" filename))        ; Lock files (.#file.org)
       (not (string-match-p "^#.*#$" filename))       ; Auto-save files (#file.org#)
       (not (string-match-p "\\.backup$" filename))   ; Explicit backup files
       (not (string-match-p "^\\." filename))         ; Any hidden files
       ;; Not in output directory
       (not (string-prefix-p (expand-file-name org-personal-site-output-dir)
                            (expand-file-name (or file ""))))))))

(defun org-personal-site--on-file-change (event)
  "Rebuild site when content changes.
EVENT is the file notification event."
  (when (and event
             (org-personal-site--should-trigger-rebuild event)
             (not org-personal-site--rebuilding))
    
    ;; Cancel existing timer
    (when org-personal-site--rebuild-timer
      (cancel-timer org-personal-site--rebuild-timer))
    
    ;; Check if enough time has passed since last rebuild (prevent loops)
    (let ((now (float-time)))
      (when (or (not org-personal-site--last-rebuild-time)
                (> (- now org-personal-site--last-rebuild-time) 2.0))
        
        ;; Schedule rebuild with debounce
        (setq org-personal-site--rebuild-timer
              (run-with-idle-timer 
               1.5 nil 
               (lambda ()
                 (setq org-personal-site--rebuilding t)
                 (org-personal-site--message "Content changed, rebuilding...")
                 (condition-case err
                     (progn
                       (org-personal-site-build)
                       (setq org-personal-site--last-rebuild-time (float-time))
                       (org-personal-site--message "Rebuild complete. Refresh your browser."))
                   (error (org-personal-site--message "Build error: %s" (error-message-string err))))
                 (setq org-personal-site--rebuilding nil))))))))

;;; Content Creation

;;;###autoload
(defun org-personal-site-new-post (title)
  "Create a new blog post with TITLE.

This command:
1. Prompts for a blog post title
2. Creates a file named YYYY-MM-DD-slug.org in content/blog/
3. Inserts a template with metadata (title, date)
4. Opens the file for editing

The filename slug is generated automatically from the title.

See also: `org-personal-site-insert-image'."
  (interactive "sBlog post title: ")
  (org-personal-site--ensure-valid)
  
  (let* ((slug (org-personal-site--slugify title))
         (date-str (format-time-string "%Y-%m-%d"))
         (filename (format "%s-%s.org" date-str slug))
         (filepath (expand-file-name filename 
                                    (expand-file-name "content/blog" 
                                                     org-personal-site-source-dir))))
    
    ;; Check if file already exists
    (when (file-exists-p filepath)
      (user-error "File already exists: %s" filepath))
    
    ;; Create file with template
    (with-temp-buffer
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: [%s %s]\n" 
                     date-str
                     (format-time-string "%a")))
      (insert "#+TAGS: \n")
      (insert "\n")
      (insert "Your content here.\n")
      (write-region (point-min) (point-max) filepath nil 'silent))
    
    ;; Open file
    (find-file filepath)
    (goto-char (point-max))
    
    (org-personal-site--message "Created new post: %s" filename)))

;;;###autoload
(defun org-personal-site-insert-image ()
  "Insert image with caption at point.

This command:
1. Prompts for an image from content/media/ directory
2. Prompts for a caption
3. Inserts org-mode syntax for the image with caption

The image path is automatically adjusted to be relative to the
blog post location (../media/filename).

See also: `org-personal-site-new-post'."
  (interactive)
  (org-personal-site--ensure-valid)
  
  (let* ((media-dir (expand-file-name "content/media" 
                                      org-personal-site-source-dir))
         (images (when (file-directory-p media-dir)
                  (directory-files media-dir nil 
                                  "\\.[png|jpg|jpeg|gif|svg]$")))
         (image (if images
                   (completing-read "Select image: " images nil t)
                 (read-string "Image filename: ")))
         (caption (read-string "Caption: ")))
    
    (insert (format "#+CAPTION: %s\n[[file:../media/%s]]\n\n" 
                    caption image))
    
    (org-personal-site--message "Image inserted: %s" image)))

;;; Deployment

;;;###autoload
(defun org-personal-site-deploy ()
  "Build site and deploy using git.

This command:
1. Builds the site
2. Stages all changes (git add -A)
3. Prompts for commit message
4. Creates commit
5. Optionally pushes to remote (based on `org-personal-site-auto-push')

The output directory must be a git repository.

See also: `org-personal-site-build'."
  (interactive)
  (org-personal-site--ensure-valid)
  
  ;; Build
  (org-personal-site-build)
  
  ;; Check if output dir is a git repo
  (let ((default-directory (expand-file-name org-personal-site-output-dir)))
    (unless (file-directory-p ".git")
      (user-error "Output directory is not a git repository: %s\n\nHint: Run 'git init' in the output directory" 
                  org-personal-site-output-dir))
    
    ;; Check if there are changes
    (let ((status-output (shell-command-to-string "git status --porcelain")))
      (if (string-empty-p status-output)
          (org-personal-site--message "No changes to deploy.")
        
        ;; Stage all changes using git
        (shell-command "git add -A")
        
        ;; Prompt for commit message
        (let ((commit-msg (read-string "Commit message: " 
                                      (format "Update site %s" 
                                             (format-time-string "%Y-%m-%d")))))
          
          ;; Create commit using git
          (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
          (org-personal-site--message "Changes committed: %s" commit-msg)
          
          ;; Push (if configured)
          (when org-personal-site-auto-push
            (if (y-or-n-p "Push to remote? ")
                (progn
                  (org-personal-site--message "Pushing to remote...")
                  (let ((push-result (shell-command-to-string "git push 2>&1")))
                    (if (string-match-p "Everything up-to-date\\|->\\|fast-forward" push-result)
                        (org-personal-site--message "Site deployed successfully!")
                      (org-personal-site--message "Push output: %s" push-result))))
              (org-personal-site--message "Site committed. Push manually when ready.")))
          
          (unless org-personal-site-auto-push
            (org-personal-site--message "Site committed. Push with 'git push' or Magit when ready.")))))))

;;; Footer

(provide 'org-personal-site)

;;; org-personal-site.el ends here
