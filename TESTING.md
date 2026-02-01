# Testing org-personal-site

This guide will help you test the org-personal-site package with your existing site.

## Prerequisites

Ensure you have the required dependencies installed:

```elisp
;; Add to your init.el if not already installed
(use-package simple-httpd
  :ensure t)

(use-package magit
  :ensure t)
```

## Step 1: Load the Package

Add this to your Emacs configuration (or evaluate in scratch buffer):

```elisp
(use-package org-personal-site
  :load-path "~/org/site/org-personal-site/"
  :custom
  (org-personal-site-source-dir "~/org/site")
  (org-personal-site-output-dir "~/site-output")
  (org-personal-site-name "nana adane")
  (org-personal-site-author "Nana Adane")
  (org-personal-site-url "https://yourdomain.com")
  (org-personal-site-theme 'opencode-tui)
  (org-personal-site-font-body-cdn 
   "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css")
  :bind (("C-c s n" . org-personal-site-new-post)
         ("C-c s b" . org-personal-site-build)
         ("C-c s p" . org-personal-site-preview)
         ("C-c s d" . org-personal-site-deploy)
         ("C-c s i" . org-personal-site-insert-image)))
```

Then reload your config or evaluate the use-package form.

## Step 2: Verify Structure

The package expects this structure in `~/org/site`:

```
~/org/site/
├── content/
│   ├── index.org          (must exist)
│   ├── blog/              (must exist)
│   │   └── *.org files
│   └── media/             (optional)
```

Your existing site already has this structure, so you're ready to go!

## Step 3: Test Build Command

```elisp
M-x org-personal-site-build
```

Expected output in minibuffer:
```
[org-personal-site] Building site...
[org-personal-site] Generated CSS: ~/site-output/links/style.css
[org-personal-site] Generated favicon: ~/site-output/favicon.svg
[org-personal-site] RSS feed generated: ~/site-output/feed.xml
[org-personal-site] Build complete in X.XXs! Output: ~/site-output
```

Check the output directory for generated files.

## Step 4: Compare Output

Compare the new output with your existing `public/` directory:

```bash
# Check if HTML structure is similar
ls ~/site-output/
ls ~/site-output/blog/

# Compare a blog post
diff ~/site-output/blog/dwarf-fortress.html public/blog/dwarf-fortress.html

# Check CSS
cat ~/site-output/links/style.css
```

## Step 5: Test Preview Mode

```elisp
M-x org-personal-site-preview
```

This should:
1. Build the site
2. Start HTTP server on port 8080
3. Open browser to http://localhost:8080
4. Watch for changes

Try editing a blog post and saving - it should auto-rebuild within ~1 second.

Stop with: `M-x org-personal-site-preview-stop`

## Step 6: Test New Post Command

```elisp
M-x org-personal-site-new-post RET Test Post RET
```

This creates `content/blog/2026-02-01-test-post.org` with template.

Verify:
- File was created
- Template includes #+TITLE, #+DATE, #+TAGS
- File opened in buffer

## Step 7: Test Image Insertion

Open a blog post and run:

```elisp
M-x org-personal-site-insert-image
```

Select an image from `content/media/` and add a caption.

Verify the inserted syntax:
```org
#+CAPTION: Your caption
[[file:../media/image.png]]
```

## Step 8: Test Deployment (Optional)

**WARNING:** This will commit and potentially push to your output directory!

First, set up a test output directory:

```bash
mkdir -p ~/site-output-test
cd ~/site-output-test
git init
```

Update your config:
```elisp
(setq org-personal-site-output-dir "~/site-output-test")
```

Then test deploy:
```elisp
M-x org-personal-site-deploy
```

Verify:
- Site was built
- Changes were staged
- Commit was created
- Push prompt appeared (if auto-push is enabled)

## Comparison with Old System

### What's the Same
- Output HTML/CSS should look identical
- Blog posts, images, RSS all work the same
- Directory structure is the same

### What's Different
- No `build.sh` script needed
- No `scripts/` directory needed
- No generated `build-site.el` or `generate-rss.el` files
- Everything is done through Emacs commands
- Live preview with auto-rebuild
- Theme is configurable without editing CSS

### Migrating Fully

Once you're satisfied with the package:

1. Delete old build scripts:
   ```bash
   rm build.sh build-site.el generate-rss.el
   rm -rf scripts/
   ```

2. Update your output directory:
   ```bash
   # If using public/ currently
   mv public site-output
   
   # Or point package to public/
   (setq org-personal-site-output-dir "~/org/site/public")
   ```

3. Add package config to your Emacs init permanently

4. (Optional) Move package to separate repo for version control

## Troubleshooting

### Package Won't Load

**Issue:** `Cannot open load file: simple-httpd`

**Fix:** Install dependencies:
```elisp
M-x package-install RET simple-httpd RET
M-x package-install RET magit RET
```

### Build Fails with "Invalid site structure"

**Issue:** Missing required directories

**Fix:** Verify these exist:
- `~/org/site/content/`
- `~/org/site/content/blog/`
- `~/org/site/content/index.org`

### Preview Server Won't Start

**Issue:** Port 8080 already in use

**Fix:** Change port:
```elisp
(setq org-personal-site-server-port 8081)
```

Or stop existing server:
```bash
# Find process using port 8080
lsof -i :8080
# Kill it
kill -9 <PID>
```

### CSS Doesn't Match

**Issue:** Generated CSS looks different

**Fix:** The package generates CSS from scratch. If you had custom CSS modifications, you'll need to:
1. Use `org-personal-site-custom-head-html` to add custom styles
2. Or modify the `org-personal-site--generate-css-content` function

### Images Missing in Blog Posts

**Issue:** Images show as broken links

**Fix:** Ensure posts use relative path:
```org
[[file:../media/image.png]]   # From blog posts
```

Not:
```org
[[file:media/image.png]]       # Wrong from blog posts
```

## Reporting Issues

If you find bugs or issues:

1. Check `*Messages*` buffer for error details
2. Try running with `M-x toggle-debug-on-error`
3. Note the exact command that failed
4. Document expected vs actual behavior

## Next Steps

After successful testing:

1. Move package to its own git repository
2. Update use-package config to use `:vc` instead of `:load-path`
3. Consider publishing to GitHub for others to use
4. Add any custom features you need

---

Happy testing!
