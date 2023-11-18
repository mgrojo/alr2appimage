# alr-appimage

`alr-appimage` is a tool to automatically create an AppImage executable from an Alire crate.

# How to use the tool

There are two prerequisites for your project to work with this tool:
- It has to be an Alire crate.
- It must be installable using `gprinstall`.

`alr-appimage` will use the following command for installing it:
 alr exec -P -- gprinstall --prefix=$(DESTDIR) --create-missing-dirs --mode=usage -f

If you simply run the tool inside an Alire crate, it will read the
metadata from your `alire.toml` file and create a default AppImage
from it.

The following metadata will be read from your `alire.toml` for the generation, and will be matched to the equivalent field of the `your_crate.desktop` file:
| Alire  | Desktop   | Comment  |
|---|---|---|
| `name` | `Name` |
| `description` | `Comment` |
| `tags` | `Keywords` | Additionally, any of the tags matching (case-insensitively) one of the main categories in the [freedesktop.org menu specification](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html) will be used as `Categories`.
| `executables` | `Exec` |
| | `Terminal` | This field of the desktop entry will be set to `true`
| | `Type` | This field of the desktop entry will be set to `Application`


