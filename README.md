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
       :vc (:url "https://github.com/cxa/lsp-biome"))
       ;; or :quelpa (lsp-biome :fetcher github :repo "cxa/lsp-biome")) if are using Emacs below 30
   ```

## Configuration

Customize the behavior of `lsp-biome` using the following configuration options:

- `lsp-biome-active-file-types`: file types that `lsp-biome` should activate, default is:
  ```elsip
  (list (rx "." (or "tsx" "jsx"
                    "ts" "js"
                    "mts" "mjs"
                    "cts" "cjs"
                    "json" "jsonc"
                    "css")
            eos)
  ```
  
- `lsp-biome-active-hook`: Hooks to run after activation, default is `nil`. See below for a usage example.

## Commands

Explore the following commands provided by `lsp-biome`:

- **`lsp-biome-organize-imports`**: Organize imports in the current buffer.
- **`lsp-biome-fix-all`**: Apply automatic code fixes in current buffer.

## Tips

Use with `apheleia`:

``` elsip
(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome")
  :preface
  (defun my/lsp-biome-active-hook ()
    (setq-local apheleia-formatter '(biome)))
  
  :config
  (add-hook 'lsp-biome-active-hook #'my/lsp-biome-active-hook))
```

## Notice

To use `lsp-biome`, you must install `biome` either within your project or globally (ensure that Emacs can locate your `biome` in the `$PATH`).

