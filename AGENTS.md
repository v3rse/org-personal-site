# org-personal-site Agent Guide

## Core Directives for AI Agents

1. **Philosophy**: This is a "batteries-included" Emacs static site generator. Prioritize "frictionless" UX, minimal dependencies, and no modification of source files.
2. **Context**: You are working in an Emacs Lisp package. Assume the user is an Emacs user.
3. **Safety**: Never modify the `content/` source directory programmatically unless explicitly asked (e.g., creating a new post). Always treat `output-dir` as ephemeral/overwriteable.

## Build, Lint, and Verify

There is no automated test suite (ERT). Verification relies on static analysis and manual testing.

### Static Analysis (Run before changes)
Always run these checks before submitting code.

1. **Syntax Check** (Catch unbalanced parens):
   ```bash
   emacs --batch --eval "(check-parens)" org-personal-site.el
   ```

2. **Byte Compilation** (Catch warnings, unused vars, free variables):
   ```bash
   emacs --batch --eval "(byte-compile-file \"org-personal-site.el\")"
   ```
   *Goal*: 0 warnings. If `package-lint` were available, we'd use it, but `byte-compile` is the baseline.

### Manual Verification
Refer to `TESTING.md` for comprehensive scenarios. Common validation steps:

- **Load Package**: 
  ```elisp
  (load-file "org-personal-site.el")
  ```
- **Run Build**: 
  `M-x org-personal-site-build` -> Check `*Messages*` buffer for success.
- **Run Preview**: 
  `M-x org-personal-site-preview` -> Verify server starts and browser opens.
- **Check Output**: 
  Verify `output-dir` contains `index.html`, `feed.xml`, `favicon.svg`, and `links/style.css`.

## Code Style & Conventions

### General Elisp
- **Indentation**: 2 spaces. No tabs.
- **Formatting**: Keep lines < 100 chars where possible.
- **Headers**: Maintain standard package headers and section comments (`;;; Section Name`).
- **Parens**: Keep closing parens on the same line (standard Lisp style).
  *Good*: `(list a b))`
  *Bad*: `(list a b) )`

### Naming
- **Prefix**: `org-personal-site-` for everything.
- **Public**: `org-personal-site-noun` or `org-personal-site-verb` (Interactive commands).
- **Private**: `org-personal-site--noun` or `org-personal-site--verb` (Helpers).
- **Variables**: `org-personal-site-setting` (public defcustoms), `org-personal-site--state` (internal).
- **Constants**: `org-personal-site--themes`.

### Documentation
- **Docstrings**: **MANDATORY** for every `defun`, `defvar`, `defcustom`.
  - Format: First line is a summary sentence. Subsequent lines detail args and behavior.
  - Interactive commands *must* document their behavior clearly for the user.
- **Comments**: Explain *intent*, not mechanics. Use inline comments sparingly.

### Imports
- **Top-level**: `(require 'feature)` at the top of the file.
- **Dynamic**: Use `(require 'feature nil 'noerror)` inside functions for optional dependencies (e.g., `htmlize`, `simple-httpd` fallback).

### Error Handling
- **User Errors**: Use `(user-error "Message")` for anticipated configuration/usage issues.
- **System Errors**: Use `(error "Message")` for bugs or unexpected states.
- **Logging**: Use `(org-personal-site--message "...")` to output with the standard prefix.

## Architecture & Internals

### File Structure
- `org-personal-site.el`: Single-file package containing all logic.
- `content/`: Source directory (Org files). Contains `index.org`, `blog/`, `media/`.
- `output-dir/`: Generated site (HTML, CSS). Treat as disposable.

### Key Components
1. **Config**: `defcustom` variables define behavior (themes, fonts, dirs).
2. **Build Engine**: 
   - Uses `org-publish` for Org->HTML conversion.
   - Dynamically constructs `org-publish-project-alist` based on user config.
   - Cleans up orphaned HTML files (files with no corresponding Org source).
3. **Preview**: 
   - Tries `python3 -m http.server` first (performance).
   - Falls back to Elisp `simple-httpd`.
   - Uses `file-notify` to watch `content/` and trigger rebuilds.
4. **Deploy**: 
   - Uses raw Git shell commands (`git add`, `git commit`).
   - Does NOT use Magit API to ensure zero-dependency deployment.

### Design Decisions
- **In-Memory Generation**: CSS, RSS, and Favicon are generated strings in Elisp, not read from template files. This keeps the package self-contained.
- **Post-Processing**: Index page is modified *after* generation to inject "Recent Posts".
- **Theme System**: Defined as an alist of plists (`:background`, `:foreground`, etc.).

## Common Workflows

### Adding a Feature
1. **Define Config**: Add `defcustom` in "Configuration" section.
2. **Implement Logic**: Create private helper `org-personal-site--helper`.
3. **Expose Command**: Create interactive `org-personal-site-command`.
4. **Document**: Update `README.md` and `AGENTS.md` if necessary.

### Fixing a Bug
1. **Reproduce**: Use `TESTING.md` steps.
2. **Fix**: Edit `org-personal-site.el`.
3. **Verify**: Run `check-parens` and `byte-compile`.
4. **Test**: Run `M-x org-personal-site-build` and verify output.

## Cursor/Copilot Rules
*No specific rules found in .cursorrules or .github/copilot-instructions.md.*
Follow the "Code Style & Conventions" above strictly.

### Example: Good vs Bad Style

**Bad**:
```elisp
(defun my-func ()
  ;; Missing docstring
  (setq some-global-var 1) ; Side effect on global
  (message "Done")) ; Generic message
```

**Good**:
```elisp
(defun org-personal-site--calculate-thing (arg)
  "Calculate the thing based on ARG.
Returns the calculated value."
  (let ((result (+ arg 1))) ; Local scope
    (org-personal-site--message "Calculation done: %d" result)
    result))
```
