# org-personal-site

A frictionless static site generator for Emacs org-mode that prioritizes ease of use and minimal configuration. Build beautiful, fast-loading websites from your org files with built-in theming, live preview, and seamless deployment via Magit.

## Features

- **Emacs-native**: Pure Elisp implementation, no external scripts required
- **Live preview**: Auto-rebuild on file changes with instant browser refresh
- **Built-in themes**: OpenCode TUI, Light, and Gruvbox themes included
- **Fully customizable**: Configure colors, fonts, and layout
- **RSS feed generation**: Automatic RSS feed from your blog posts
- **Magit integration**: One-command deployment to GitHub Pages
- **Image helpers**: Easy image insertion with captions
- **Smart date handling**: Supports both `<YYYY-MM-DD>` and `[YYYY-MM-DD]` formats
- **Draft support**: Posts with `#+DRAFT: t` are excluded from builds
- **Tag support**: Organize posts with `#+TAGS:`

## Installation

### Local Development

Clone or copy this package to a local directory, then add to your Emacs configuration:

```elisp
(use-package org-personal-site
  :load-path "~/path/to/org-personal-site/"
  :custom
  (org-personal-site-source-dir "~/org/my-blog")
  (org-personal-site-output-dir "~/my-site.github.io")
  (org-personal-site-name "My Site")
  (org-personal-site-author "Your Name"))
```

### From Git Repository (after publishing)

```elisp
(use-package org-personal-site
  :vc (:fetcher github :repo "username/org-personal-site")
  :custom
  (org-personal-site-source-dir "~/org/my-blog")
  (org-personal-site-output-dir "~/my-site.github.io")
  (org-personal-site-name "My Site")
  (org-personal-site-author "Your Name"))
```

## Quick Start

### 1. Create Directory Structure

Set up your source directory with the required structure:

```
~/org/my-blog/
├── content/
│   ├── index.org          (homepage - required)
│   ├── projects.org       (projects page - optional)
│   ├── blog/              (blog posts - required)
│   │   └── first-post.org
│   └── media/             (images - optional)
│       └── photo.png
```

**Minimal `content/index.org`:**

```org
#+TITLE: About Me

Hi, I'm Your Name.

I am a software engineer and writer.

** Recent Writing

** Projects

Check out my [[file:projects.html][Projects]].
```

The "Recent Writing" section will be automatically populated with your latest blog posts during the build. The package injects the list into the generated HTML without modifying your source files.

### 2. Configure the Package

Add to your Emacs configuration:

```elisp
(use-package org-personal-site
  :load-path "~/path/to/org-personal-site/"
  :custom
  ;; Required settings
  (org-personal-site-source-dir "~/org/my-blog")
  (org-personal-site-output-dir "~/my-site.github.io")
  (org-personal-site-name "your name")
  (org-personal-site-author "Your Name")
  
  ;; Optional: Use Iosevka font from cdnjs
  (org-personal-site-font-body-cdn 
   "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css")
  
  ;; Optional: Keybindings
  :bind (("C-c s n" . org-personal-site-new-post)
         ("C-c s b" . org-personal-site-build)
         ("C-c s p" . org-personal-site-preview)
         ("C-c s d" . org-personal-site-deploy)
         ("C-c s i" . org-personal-site-insert-image)))
```

### 3. Create Your First Post

```elisp
M-x org-personal-site-new-post RET My First Post RET
```

This creates `content/blog/YYYY-MM-DD-my-first-post.org` with a template:

```org
#+TITLE: My First Post
#+DATE: [2026-02-01 Sat]
#+TAGS: 

Your content here.
```

### 4. Build and Preview

```elisp
M-x org-personal-site-preview
```

This will:
- Build your site
- Start a local HTTP server
- Open your browser to preview
- Watch for file changes and auto-rebuild

Press `M-x org-personal-site-preview-stop` to stop the server.

### 5. Deploy to GitHub Pages

First, set up your output directory as a git repository:

```bash
cd ~/my-site.github.io
git init
git remote add origin git@github.com:username/username.github.io.git
```

Then deploy:

```elisp
M-x org-personal-site-deploy
```

This will build, commit, and optionally push your changes.

## Directory Structure

### Source Directory

```
source-dir/
├── content/
│   ├── index.org          # Homepage (required)
│   ├── projects.org       # Projects page (optional)
│   ├── blog/              # Blog posts directory (required)
│   │   ├── 2026-02-01-first-post.org
│   │   └── 2025-12-25-another-post.org
│   └── media/             # Images and assets (optional)
│       ├── photo.png
│       └── diagram.svg
```

### Output Directory (Generated)

```
output-dir/
├── index.html             # Generated from content/index.org
├── projects.html          # Generated from content/projects.org
├── blog/
│   ├── index.html         # Auto-generated blog index
│   ├── 2026-02-01-first-post.html
│   └── 2025-12-25-another-post.html
├── media/                 # Copied from source
│   ├── photo.png
│   └── diagram.svg
├── links/
│   └── style.css          # Generated from theme config
├── feed.xml               # Generated RSS feed
└── favicon.svg            # Generated from site name
```

## Configuration Reference

### Required Settings

| Variable | Description | Example |
|----------|-------------|---------|
| `org-personal-site-source-dir` | Source directory with org files | `"~/org/my-blog"` |
| `org-personal-site-output-dir` | Output directory (git repo) | `"~/username.github.io"` |
| `org-personal-site-name` | Site name (appears in header) | `"my site"` |
| `org-personal-site-author` | Author name for metadata | `"Your Name"` |

### Optional Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `org-personal-site-url` | Site URL for RSS feed | `""` |
| `org-personal-site-description` | Site description for RSS | `""` |
| `org-personal-site-cname` | Custom domain for GitHub Pages | `""` |
| `org-personal-site-theme` | Theme preset or custom config | `opencode-tui` |
| `org-personal-site-font-logo` | Logo font | `"Pixelify Sans"` |
| `org-personal-site-font-heading` | Heading font | `"JetBrains Mono"` |
| `org-personal-site-font-body` | Body text font | `"Iosevka Etoile"` |
| `org-personal-site-font-body-cdn` | Custom CDN for body font | `nil` |
| `org-personal-site-custom-head-html` | Additional HTML for `<head>` | `""` |
| `org-personal-site-server-port` | Preview server port | `8080` |
| `org-personal-site-use-python-server` | Use Python instead of simple-httpd | `t` |
| `org-personal-site-auto-push` | Prompt to push when deploying | `t` |
| `org-personal-site-favicon-letter` | Favicon letter (auto if nil) | `nil` |
| `org-personal-site-favicon-font` | Favicon font | `"Pixelify Sans"` |
| `org-personal-site-recent-posts-count` | Recent posts on homepage | `5` |

## Commands Reference

### Content Creation

#### `org-personal-site-new-post`
Create a new blog post with an interactive prompt for the title.

- Generates filename: `YYYY-MM-DD-slug.org`
- Creates file in `content/blog/`
- Pre-populates with template
- Opens file for editing

**Keybinding suggestion:** `C-c s n`

#### `org-personal-site-insert-image`
Insert an image with caption at point.

- Prompts for image from `content/media/`
- Prompts for caption
- Inserts org syntax with relative path
- Automatically adjusts path for blog posts (`../media/`)

**Keybinding suggestion:** `C-c s i`

### Building & Preview

#### `org-personal-site-build`
Build the entire site from source to output directory.

Steps:
1. Validates directory structure
2. Generates CSS from theme
3. Converts org files to HTML
4. Generates RSS feed
5. Creates favicon

**Keybinding suggestion:** `C-c s b`

#### `org-personal-site-preview`
Build site, start HTTP server, and watch for changes.

Features:
- Builds site on start
- Serves on `http://localhost:8080` (configurable)
- Opens browser automatically
- Watches `content/`, `content/blog/`, and `content/media/` for changes
- Auto-rebuilds on file save (debounced, 1.5s delay)
- Smart filtering excludes:
  - Auto-save files (`#file.org#`)
  - Lock files (`.#file.org`)
  - Backup files (`file.org~`, `*.backup`)
  - Hidden files (`.` prefix)
  - Non-org and non-media files
- Prevents rebuild loops with 2-second minimum between rebuilds

Stop with `M-x org-personal-site-preview-stop`.

**Keybinding suggestion:** `C-c s p`

#### `org-personal-site-preview-stop`
Stop the preview server and file watcher.

### Deployment

#### `org-personal-site-deploy`
Build site and deploy using Magit.

Steps:
1. Builds the site
2. Stages all changes in output directory
3. Prompts for commit message
4. Creates commit
5. Optionally pushes to remote

Requirements:
- Output directory must be a git repository
- Remote must be configured (for push)

**Keybinding suggestion:** `C-c s d`

## Customization

### Using Built-in Themes

```elisp
;; OpenCode TUI (dark, warm, terminal-inspired)
(setq org-personal-site-theme 'opencode-tui)

;; Light theme
(setq org-personal-site-theme 'light)

;; Gruvbox dark
(setq org-personal-site-theme 'gruvbox)
```

### Custom Theme

```elisp
(setq org-personal-site-theme
      '(:background "#1e1e1e"
        :foreground "#d4d4d4"
        :accent "#569cd6"))
```

### Custom Fonts

```elisp
;; All fonts from Google Fonts
(setq org-personal-site-font-logo "Roboto Mono"
      org-personal-site-font-heading "Inter"
      org-personal-site-font-body "Source Sans Pro")

;; Body font from custom CDN
(setq org-personal-site-font-body "Iosevka Etoile"
      org-personal-site-font-body-cdn
      "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css")
```

### Custom Favicon

```elisp
;; Use specific letter
(setq org-personal-site-favicon-letter "X")

;; Auto-use first letter of site name (default)
(setq org-personal-site-favicon-letter nil)

;; Custom font for favicon
(setq org-personal-site-favicon-font "Courier New")
```

### Adding Custom HTML to Head

```elisp
(setq org-personal-site-custom-head-html
      "<meta name=\"google-site-verification\" content=\"...\"/>
<script async src=\"https://www.googletagmanager.com/gtag/js?id=...\"></script>")
```

### Deployment Behavior

```elisp
;; Always prompt before pushing
(setq org-personal-site-auto-push t)

;; Never auto-push (commit only, push manually)
(setq org-personal-site-auto-push nil)
```

## Blog Post Metadata

### Supported Metadata

```org
#+TITLE: My Post Title
#+DATE: [2026-02-01 Sat]      # Can also use <2026-02-01>
#+TAGS: emacs, org-mode        # Comma-separated tags
#+DRAFT: t                     # Exclude from build (t, true, yes)
```

### Date Formats

Both formats are supported:

```org
#+DATE: [2026-02-01 Sat]   # With day of week
#+DATE: <2026-02-01>       # Without day of week
```

### Draft Posts

Posts marked as drafts are excluded from:
- Blog index
- Homepage recent posts
- RSS feed

```org
#+DRAFT: t
```

Remove the `#+DRAFT` line or set to `nil` to publish.

## GitHub Pages Deployment

### Basic Setup (username.github.io)

1. Create a repository named `username.github.io`
2. Configure the package:

```elisp
(setq org-personal-site-output-dir "~/username.github.io")
```

3. Deploy:

```elisp
M-x org-personal-site-deploy
```

### Custom Domain Setup

If you have a custom domain (e.g., `example.com`):

1. Add domain to configuration:

```elisp
(setq org-personal-site-cname "example.com"
      org-personal-site-url "https://example.com")
```

2. The package will automatically generate a `CNAME` file during build

3. Configure DNS with your domain registrar:
   - For apex domain (`example.com`): Add A records pointing to GitHub's IPs
   - For subdomain (`www.example.com`): Add CNAME record pointing to `username.github.io`

4. In GitHub repository settings:
   - Go to Settings → Pages
   - Enter your custom domain
   - Enable "Enforce HTTPS"

See [GitHub Pages documentation](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site) for detailed DNS setup.

## Workflow Examples

### Writing a New Post

```elisp
;; 1. Create post
M-x org-personal-site-new-post RET "Understanding Emacs Lisp" RET

;; 2. Write content
;; ... edit the file ...

;; 3. Add an image
M-x org-personal-site-insert-image RET screenshot.png RET "My screenshot" RET

;; 4. Preview changes
M-x org-personal-site-preview

;; 5. Deploy when ready
M-x org-personal-site-deploy RET "Add post about Emacs Lisp" RET
```

### Updating Site Style

```elisp
;; 1. Change theme in config
(setq org-personal-site-theme 'gruvbox)

;; 2. Rebuild
M-x org-personal-site-build

;; 3. Preview
M-x org-personal-site-preview

;; 4. Deploy if satisfied
M-x org-personal-site-deploy
```

### Adding a New Page

```elisp
;; 1. Create org file
;; Edit: ~/org/my-blog/content/about.org

;; 2. Add content
#+TITLE: About
# This is my about page.

;; 3. Link from index.org or navigation
;; (You'll need to customize the navigation in the package or use includes)

;; 4. Build
M-x org-personal-site-build
```

## Troubleshooting

### "Invalid site structure" Error

**Problem:** Missing required directories or files.

**Solution:** Ensure your source directory has:
- `content/` directory
- `content/blog/` directory
- `content/index.org` file

Create them if missing:

```bash
mkdir -p ~/org/my-blog/content/blog
touch ~/org/my-blog/content/index.org
```

### Preview Server Slows Down Emacs

**Problem:** Emacs becomes sluggish during preview.

**Solution:** Use Python's HTTP server instead of simple-httpd (default):
```elisp
(setq org-personal-site-use-python-server t)  ; Default, recommended
```

If you prefer pure Elisp (slower but no Python dependency):
```elisp
(setq org-personal-site-use-python-server nil)
```

### Preview Not Auto-Rebuilding

**Problem:** File changes don't trigger rebuilds.

**Solutions:**
- Ensure you're editing files in the `content/` directory
- Check for error messages in `*Messages*` buffer
- Try stopping and restarting preview: `M-x org-personal-site-preview-stop` then `M-x org-personal-site-preview`

### Deployment Fails to Push

**Problem:** Push fails during deployment.

**Solutions:**
- Ensure git remote is configured: `git remote -v` in output directory
- Check SSH keys or credentials for GitHub
- Set `org-personal-site-auto-push` to `nil` and push manually with Magit

### Images Not Displaying

**Problem:** Images show as broken links.

**Solutions:**
- Ensure images are in `content/media/` directory
- Use relative path: `[[file:../media/image.png]]` from blog posts
- Use absolute path: `[[file:media/image.png]]` from root pages
- Rebuild: `M-x org-personal-site-build`

### Custom Font Not Loading

**Problem:** Custom body font doesn't appear.

**Solutions:**
- Verify CDN URL is correct
- Check browser console for loading errors
- Ensure `org-personal-site-font-body-cdn` is set correctly
- Try using a Google Fonts alternative

### RSS Feed Issues

**Problem:** Feed validation fails or items missing.

**Solutions:**
- Ensure `org-personal-site-url` is set (required for valid RSS)
- Check post dates are properly formatted
- Verify posts aren't marked as drafts
- Validate feed at https://validator.w3.org/feed/

## Advanced Configuration

### Multiple Site Navigation Items

To add more navigation items, you'll need to customize the navigation. Currently, the navigation is hardcoded to `bio`, `blog`, and `projects`. You can modify the source to add more items or create a custom navigation configuration.

### Custom Sitemap Format

The blog index format is defined in `org-personal-site--sitemap-entry`. You can customize this by modifying the function or using advice:

```elisp
(defun my-custom-sitemap-entry (entry style project)
  "Custom sitemap entry format."
  (when (not (directory-name-p entry))
    (format "%s - [[file:%s][%s]]"
            (format-time-string "%b %d, %Y" 
                              (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))

(advice-add 'org-personal-site--sitemap-entry :override #'my-custom-sitemap-entry)
```

### Performance Optimization

For sites with many posts, you can optimize build times by:

1. Using org-publish cache (enabled by default)
2. Building only changed files: `(org-publish "blog" nil)` instead of `(org-publish-all t)`
3. Reducing image sizes in `content/media/`

## Dependencies

- **Emacs 27.1+**: Required for modern org-mode and file notifications
- **simple-httpd**: HTTP server for preview mode (optional if using Python server)
- **Git**: Required for deployment
- **Magit**: Optional, for enhanced git workflow

Install dependencies via package.el or use-package:

```elisp
(use-package simple-httpd
  :ensure t)

;; Magit is optional but recommended for git workflow
(use-package magit
  :ensure t)
```

## Development

### Contributing

This package is in active development. Contributions, bug reports, and feature requests are welcome!

### Package Structure

- Single file package (~800 lines)
- All functionality self-contained
- No external scripts required
- Uses standard Emacs libraries

### Testing

Test locally by loading the package and running commands on a test site:

```elisp
(load-file "~/path/to/org-personal-site/org-personal-site.el")
```

## License

This package is free software; you can redistribute it and/or modify it under the terms of your choice.

## Acknowledgments

- Built with Emacs org-mode's excellent publishing system
- Inspired by minimalist static site generators
- OpenCode TUI theme inspired by terminal aesthetics

## Support

For issues, questions, or feedback:
- Check the troubleshooting section above
- Review error messages in `*Messages*` buffer
- Consult org-mode documentation for publishing options

---

**Made with ❤️ and Emacs**
