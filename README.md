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

If your crate have resources, it is recommended to use the `resources`
crate or a similar mechanism to properly load the resource files from
the installation prefix.

The utility [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)
is automatically downloaded and used. For the download, having `curl`
or `wget` installed is required.

| Alire  | Desktop   | Comment  |
|---|---|---|
| `name` | `Name` |
| `description` | `Comment` |
| `tags` | `Keywords` | Additionally, any of the tags matching (case-insensitively) one of the main categories in the [freedesktop.org menu specification](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html) will be used as `Categories`.
| `executables` | `Exec` | The first executable is used as entrypoint of the AppImage.
| | `Terminal` | This field of the desktop entry will be set to `false` by default. It can be set to `true` passing the `--terminal` argument.
| | `Type` | This field of the desktop entry will be set to `Application`.
| | `Icon` | By default, the included icon (`alr_appimage.png`) will be used. It can be orverriden using the `--icon your-icon-file` argument.

# Usage
```
Usage: alr_appimage [OPTIONS]...
Makes an AppImage from your Alire crate.

  -h, --help       Display this help text.
  -V, --version    Display the version of this utility.
  -t, --terminal   Set the Terminal flag of the AppImage to true, i.e. the application is for the terminal or requires to be run from a terminal.
  -i, --icon       Specify the icon file for the AppImage
```

# Status
The tool is already working, but with the following limitations, which will
be fixed in future versions:
- The executable is supposed to reside in `bin/` and be called as the
  crate name. In the future, the same logic that `alr` is using will be followed.
- Not implemented yet: If the defaults are not good for your
  project, you can also define a template for your desktop file, and
  only the fields that you want will be included from the `alire.toml`
  or initialized by this utility.
