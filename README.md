![Alr_Appimage](https://raw.githubusercontent.com/mgrojo/alr-appimage/master/share/alr_appimage/alr_appimage.png "alr_appimage icon")

# alr_appimage

`alr_appimage` is a tool to automatically create an
[AppImage](https://appimage.org/) executable from an Alire crate.

# How to use the tool

There are two prerequisites for your project to work with this tool:
- It has to be a crate with an `executables` field. Its first value
  has to be the main application program.
- It must be installable using Alire, including all the needed resources.

`alr_appimage` will use the following command for installing it (this requires Alire 2.0):
```shell
alr install
```

If you simply run the tool inside an Alire crate, it will read the
metadata from your `alire.toml` file and create a default AppImage
from it.

The utility [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)
is automatically downloaded using `curl` or `wget` tools. If you
cannot install one of these utilities or lack an Internet connection in the
host, just put a copy of the `linuxdeploy-your_arch.AppImage` in the
crate directory, and it will be used by `alr_appimage`.

This tool will generate a valid [Destop Entry](https://specifications.freedesktop.org/desktop-entry-spec/latest/)
for the AppImage. The following metadata will be read from your
`alire.toml` for the generation, and will be matched to the equivalent
field of the `your_crate.desktop` file:

| Alire  | Desktop   | Comment  |
|---|---|---|
| `name` | `Name` | It will also be the base name of your AppImage file.
| `description` | `Comment` |
| `tags` | `Keywords` | Additionally, any of the tags matching (case-insensitively) one of the main categories in the [freedesktop.org menu specification](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html) will be used as `Categories`.
| `executables` | `Exec` | The first executable is used as entrypoint of the AppImage.
| | `Terminal` | This field of the desktop entry will be set to `false` by default. It can be set to `true` passing the `--terminal` argument.
| | `Type` | This field of the desktop entry will be set to `Application`.
| | `Icon` | By default, the included icon (`alr_appimage.png`) will be used. It can be orverriden using the `--icon your-icon-file` argument.

If your crate have resources, it is recommended to use the `resources`
crate, or a similar mechanism, to properly load the resource files from
the installation prefix.

# Usage
```
Usage: alr_appimage [OPTIONS]...
Makes an AppImage from your Alire crate.

  -h, --help       Display this help text.
  -V, --version    Display the version of this utility.
  -t, --terminal   Set the Terminal flag of the AppImage to true, i.e. the application is for the terminal or requires to be run from a terminal.
  -i, --icon       Specify the icon file for the AppImage
```

Run inside an Alire crate to create a default AppImage of your
application.  You can provide your own icon and override the default
value (false) for the terminal field.


# Status
The tool considered complete, although it could be developed further.

The following feature could be implemented in the future, if deemed
useful for the users:
- Not implemented yet: If the defaults are not good for your
  project, you can also define a template for your desktop file, and
  only the fields that you want will be included from the `alire.toml`
  or initialized by this utility.

# Building
Can be built and installed after cloning the repository using Alire.
