# Installation Guide

## Package Location

The package is now in: `~/src/org-personal-site/`

## Installation in Emacs

### Option 1: Local Installation (Current Setup)

Add to your Emacs configuration:

```elisp
(use-package org-personal-site
  :load-path "~/src/org-personal-site/"
  :custom
  (org-personal-site-source-dir "~/org/site")
  (org-personal-site-output-dir "~/site-output")  ; Or your output directory
  (org-personal-site-name "nana adane")
  (org-personal-site-author "Nana Adane")
  (org-personal-site-url "https://yourdomain.com")
  (org-personal-site-cname "yourdomain.com")  ; If using custom domain
  (org-personal-site-font-body-cdn 
   "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css")
  :bind (("C-c s n" . org-personal-site-new-post)
         ("C-c s b" . org-personal-site-build)
         ("C-c s p" . org-personal-site-preview)
         ("C-c s d" . org-personal-site-deploy)
         ("C-c s i" . org-personal-site-insert-image)))
```

### Option 2: Git Repository Installation

After pushing to GitHub:

```elisp
(use-package org-personal-site
  :vc (:fetcher github :repo "username/org-personal-site")
  :custom
  (org-personal-site-source-dir "~/org/site")
  (org-personal-site-output-dir "~/site-output")
  ;; ... other settings
  )
```

## Setting Up Git Repository

If you want to publish this package:

```bash
cd ~/src/org-personal-site

# Initialize git if not already done
git init

# Add all files
git add .

# Create initial commit
git commit -m "Initial commit: org-personal-site package"

# Add remote (create repo on GitHub first)
git remote add origin git@github.com:yourusername/org-personal-site.git

# Push to GitHub
git push -u origin main
```

## Dependencies

Install required dependencies:

```elisp
;; In Emacs
M-x package-install RET simple-httpd RET
```

Or add to your config:

```elisp
(use-package simple-httpd
  :ensure t)
```

## Verifying Installation

After adding to your config, reload and test:

```elisp
;; Reload config
M-x eval-buffer

;; Test build
M-x org-personal-site-build

;; Test preview
M-x org-personal-site-preview
```

## Directory Structure

Your setup should now be:

```
~/src/org-personal-site/     (the package)
├── org-personal-site.el
├── README.md
├── TESTING.md
├── INSTALL.md               (this file)
└── test-config.el

~/org/site/                  (your source content)
├── content/
│   ├── index.org
│   ├── projects.org
│   ├── blog/
│   └── media/
└── .git/

~/site-output/               (generated HTML)
├── index.html
├── blog/
├── media/
├── links/style.css
├── favicon.svg
├── feed.xml
├── CNAME
└── .git/
```

## Troubleshooting

### Package not found

Make sure the `:load-path` points to the correct directory:

```elisp
;; Check if directory exists
(file-directory-p "~/src/org-personal-site/")  ; Should return t
```

### Commands not available

Reload your configuration:

```elisp
M-x eval-buffer
```

Or restart Emacs.

### Build errors

Check your source directory structure:

```bash
ls -la ~/org/site/content/
# Should show: index.org, blog/, media/ (optional)
```

## Next Steps

1. Clean up old files in ~/org/site (see cleanup guide in package)
2. Test all commands
3. Set up your output directory as a git repo
4. Deploy!

See README.md for full documentation.
