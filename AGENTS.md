# org-personal-site Agent File

## Project Overview

`org-personal-site` is an Emacs package for generating static websites from org-mode files. It's designed for frictionless personal blogging and publishing.

**Location:** `~/src/org-personal-site/`

**Version:** 0.1.0

**Author:** Nana Adane

## Purpose

A batteries-included static site generator that:
- Works entirely within Emacs
- Uses org-mode as the source format
- Generates clean, fast HTML
- Provides live preview with auto-rebuild
- Deploys to GitHub Pages via git
- Requires minimal configuration

## Architecture

### Core Files

- **org-personal-site.el** (~1100 lines) - Main package
  - Configuration system (defcustom)
  - Build engine (org-publish integration)
  - Template generation (CSS, HTML, RSS, favicon)
  - Preview mode (file watching + HTTP server)
  - Content creation (new posts, image insertion)
  - Deployment (git integration)

- **README.md** - User documentation
- **TESTING.md** - Testing guide for development
- **INSTALL.md** - Installation instructions
- **test-config.el** - Example configuration

### Key Components

#### 1. Configuration System
All settings use Emacs `defcustom` for easy customization:
- Source/output directories
- Site metadata (name, author, URL)
- Theme system (3 presets + custom)
- Font configuration (Google Fonts + custom CDN)
- Server settings (port, Python vs simple-httpd)
- Deployment settings (auto-push)

#### 2. Build System
- Uses `org-publish` for org → HTML conversion
- Generates CSS from theme configuration
- Post-processes HTML to inject recent posts
- Generates RSS feed with proper XML escaping
- Creates SVG favicon from site name
- Optional CNAME file for custom domains
- Cleans up orphaned HTML files (deleted source files)

#### 3. Preview Mode
- Python HTTP server (default) or simple-httpd
- File watcher on `content/`, `content/blog/`, `content/media/`
- Smart filtering (ignores temp/backup files)
- Debounced rebuilds (1.5s delay)
- Loop prevention (2s minimum between rebuilds)

#### 4. Content Creation
- `org-personal-site-new-post` - Creates dated org file with template
- `org-personal-site-insert-image` - Inserts image with caption
- Automatic slug generation from titles

#### 5. Deployment
- Git-based (no Magit dependency)
- Stages all changes (`git add -A`)
- Prompts for commit message
- Optional auto-push
- Works with GitHub Pages

## Design Philosophy

### Principles

1. **Emacs-native** - Pure Elisp, no external scripts
2. **Minimal dependencies** - Just Emacs, simple-httpd, git
3. **No source modification** - Never modifies source files
4. **Clean separation** - Source (org) vs Output (HTML) directories
5. **Frictionless workflow** - Everything from Emacs
6. **Configurable** - But with sensible defaults

### Technical Decisions

**In-memory template generation** - CSS, HTML, RSS are generated in memory from config, not written to source directory. Cleaner and prevents stale files.

**Post-processing over preprocessing** - Instead of modifying source org files, we post-process generated HTML (e.g., injecting recent posts). Source files stay pristine.

**Git over Magit** - Direct git commands instead of Magit functions for portability and to avoid version-specific APIs.

**Python server by default** - Python's http.server is faster and doesn't slow down Emacs, but simple-httpd is available as fallback.

**Theme constraints** - Only 3 colors (background, foreground, accent) to encourage thoughtful design and maintain cohesion.

## Common Tasks

### Adding a New Feature

1. Add configuration variable if needed (defcustom in Configuration section)
2. Implement the function
3. Add interactive command (defun with ;;;###autoload)
4. Document in README.md
5. Add to test-config.el example if relevant
6. Update TESTING.md with test scenario

### Fixing a Bug

1. Identify the issue location
2. Add test case to reproduce
3. Fix the code
4. Verify syntax: `emacs --batch --eval "(check-parens)" org-personal-site.el`
5. Test with real site
6. Document in TESTING.md if edge case

### Updating CSS Generation

CSS is generated in `org-personal-site--generate-css-content`. The function:
- Takes theme colors from configuration
- Injects them into CSS template string
- Returns complete CSS as string

To modify:
1. Edit the format string in the function
2. Test with different themes
3. Verify generated CSS in output directory

### Adding a New Theme

1. Add theme definition to `org-personal-site--themes` constant
2. Include `:background`, `:foreground`, `:accent`, `:description`
3. Update theme choice in `org-personal-site-theme` defcustom
4. Document in README.md
5. Test CSS generation with new theme

## File Structure Expectations

### Source Directory

```
source-dir/
├── content/
│   ├── index.org          (required - homepage)
│   ├── projects.org       (optional)
│   ├── blog/              (required - blog posts)
│   │   └── *.org
│   └── media/             (optional - images)
│       └── *.{png,jpg,gif,svg}
```

### Output Directory

```
output-dir/
├── index.html             (generated from content/index.org)
├── projects.html          (generated from content/projects.org)
├── blog/
│   ├── index.html         (auto-generated sitemap)
│   └── *.html             (generated from blog/*.org)
├── media/                 (copied from content/media/)
├── links/
│   └── style.css          (generated from theme)
├── favicon.svg            (generated from site name)
├── feed.xml               (generated RSS)
└── CNAME                  (optional, if custom domain set)
```

## Dependencies

### Required
- Emacs 27.1+
- simple-httpd (for preview)
- Git (for deployment)
- Python 3 (for default preview server)

### Optional
- htmlize (for syntax highlighting in code blocks)

## Known Limitations

1. **No nested blog directories** - All posts must be directly in `content/blog/`
2. **Single theme at a time** - No per-page theme overrides
3. **English only RSS** - Language hardcoded to "en"
4. **No image optimization** - Images copied as-is
5. **Basic sitemap** - Date + title only, no tags in sitemap

## Testing

Run syntax check:
```bash
emacs --batch --eval "(check-parens)" org-personal-site.el
```

Test with example site:
```elisp
;; Load package
(load-file "~/src/org-personal-site/org-personal-site.el")

;; Test build
M-x org-personal-site-build

;; Test preview
M-x org-personal-site-preview
```

See TESTING.md for comprehensive test scenarios.

## Debugging

### Enable verbose messages
Check `*Messages*` buffer for build output.

### Check generated files
After build, inspect:
- `output-dir/links/style.css` - Verify CSS generation
- `output-dir/feed.xml` - Verify RSS structure
- `output-dir/index.html` - Verify recent posts injection

### Preview not rebuilding
1. Check `*Messages*` for watcher errors
2. Verify file is .org and in watched directory
3. Ensure file not matching ignore patterns
4. Check rebuild timestamps are > 2s apart

### Deployment failures
1. Verify output directory is git repo: `ls output-dir/.git`
2. Check git remote is configured: `git remote -v`
3. Test git manually: `cd output-dir && git status`

## Code Style

- **Line length:** No hard limit, but keep readable (~80-100 preferred)
- **Indentation:** 2 spaces (Emacs Lisp standard)
- **Docstrings:** Required for all defun, especially interactive commands
- **Comments:** Explain "why", not "what"
- **Naming:**
  - Public: `org-personal-site-command`
  - Internal: `org-personal-site--helper`
  - Variables: `org-personal-site-setting`
  - Internal vars: `org-personal-site--state`

## Release Checklist

Before releasing a new version:

- [ ] Update version in package header
- [ ] Update CHANGELOG (if exists)
- [ ] Test all interactive commands
- [ ] Test with all 3 built-in themes
- [ ] Verify README examples work
- [ ] Check syntax: `(check-parens)`
- [ ] Test on clean Emacs instance
- [ ] Update TESTING.md if needed

## Future Ideas

These are not commitments, just ideas to consider:

- Tag support in blog index
- Multiple blog directories
- Draft preview mode
- Image optimization
- Multi-language support
- Per-page theme overrides
- Export to other formats (PDF, ePub)
- Integration with other org tools

## Support

This is a personal project. Issues and improvements welcome, but no guarantees on timeline or acceptance.

## Context for AI Agents

When working on this package:

1. **Preserve the philosophy** - Keep it simple, Emacs-native, frictionless
2. **Don't add dependencies** unless absolutely necessary
3. **Test changes** with real org-mode files
4. **Document everything** - Users should understand what's happening
5. **Maintain backwards compatibility** when possible
6. **Keep functions focused** - Do one thing well
7. **Error messages** should be helpful and actionable

The goal is a tool that gets out of the way and lets you write.

---

*Last updated: 2026-02-01*
