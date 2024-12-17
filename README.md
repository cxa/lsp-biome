# lsp-biome

**lsp-mode Client for [Biome](https://biomejs.dev/)**

## Installation

To integrate `lsp-biome` into your Emacs setup, you can take either:

1. Clone this repo, add the `lsp-biome.el` file to your Emacs `load-path`.

   ```elisp
   (add-to-list 'load-path "/path/to/lsp-biome/")
   ```

   Replace "/path/to/lsp-biome/" with the actual path to the `lsp-biome` directory.

2. Alternatively, if you're using a package manager like `use-package` with `quelpa`, add the following configuration:

   ```elisp
   (use-package lsp-biome
       :quelpa (lsp-biome :fetcher github :repo "cxa/lsp-biome"))
       ;; or :vc (:url "https://github.com/cxa/lsp-biome") if are using Emacs 30+
   ```

## Configuration

Customize the behavior of `lsp-biome` using the following configuration options:

- **lsp-biome-active-file-types**: File types that `lsp-biome` should activate. The default is:
  ```elsip
  (list (rx "." (or "tsx" "jsx"
                    "ts" "js"
                    "mts" "mjs"
                    "cts" "cjs"
                    "json" "jsonc"
                    "css")
            eos)
  ```
- **lsp-biome-organize-imports-on-save**: Enable/disable automatic organization of imports on save. The default is `nil`.
- **lsp-biome-autofix-on-save**: Enable/disable automatic fixes on save. The default is `nil`.
- **lsp-biome-format-on-save**: Enable/disable automatic formatting on save. The default is `nil`.

## Commands

Explore the following commands provided by `lsp-biome`:

- **`lsp-biome-organize-imports`**: Organize imports in the current buffer. Note: This overrides `lsp-organize-imports` under `lsp-biome`, and they are equivalent.
- **`lsp-biome-fix-all`**: Apply automatic code fixes in current buffer.

## Notice

Please note that `lsp-biome` only works with projects that have `biome` installed locally (a.k.a `node_modules` inside project).

