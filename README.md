# alr_appimage

`alr_appimage` is a tool to automatically create an AppImage executable from an Alire crate.

# How to use the tool

There are two prerequisites for your project to work with this tool:
- It has to be an Alire crate.
- It must be installable using Alire.

`alr_appimage` will use the following command for installing it (this requires Alire 2.0):
```shell
alr install
```

If you simply run the tool inside an Alire crate, it will read the
metadata from your `alire.toml` file and create a default AppImage
from it.

This tool will generate a valid [Destop Entry](https://specifications.freedesktop.org/desktop-entry-spec/latest/)
for the AppImage. The following metadata will be read from your
`alire.toml` for the generation, and will be matched to the equivalent
field of the `your_crate.desktop` file:

| Alire  | Desktop   | Comment  |
|---|---|---|
| `name` | `Name` |
| `description` | `Comment` |
| `tags` | `Keywords` | Additionally, any of the tags matching (case-insensitively) one of the main categories in the [freedesktop.org menu specification](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html) will be used as `Categories`.
| `executables` | `Exec` |
| | `Terminal` | This field of the desktop entry will be set to `true`
| | `Type` | This field of the desktop entry will be set to `Application`

# Status
The tool is already working, but with the following issues, which will
be fixed in the future:
- No resources included, so you have to provide your own icon using
  `--icon icon-file` or copy `alr_appimage.png` to the working
  directory (since it is the default).
- The executable is supposed to reside in `bin/` and be called as the
  crate name.
- Not implemented yet: If the defaults are not good for your
  project, you can also define a template for your desktop file, and
  only the fields that you want will be included from the `alire.toml`
  or initialized by this utility.
