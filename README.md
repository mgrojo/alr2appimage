[![Build](https://github.com/mgrojo/alr2appimage/actions/workflows/main.yml/badge.svg)](https://github.com/mgrojo/alr2appimage/actions/workflows/main.yml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/alr2appimage.json)](https://alire.ada.dev/crates/alr2appimage.html)
[![Alire CI/CD](https://img.shields.io/endpoint?url=https://alire-crate-ci.ada.dev/badges/alr2appimage.json)](https://alire-crate-ci.ada.dev/crates/alr2appimage.html)
[![Download][download-img]][download]
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/ada-lang/Lobby)
[![Mentioned in Awesome Ada](https://awesome.re/mentioned-badge.svg)](https://github.com/ohenley/awesome-ada)

  [download-img]: https://img.shields.io/github/downloads/mgrojo/alr2appimage/total.svg
  [download]: https://github.com/mgrojo/alr2appimage/releases
  
![Alr2AppImage](https://raw.githubusercontent.com/mgrojo/alr2appimage/master/share/alr2appimage/alr2appimage.png "alr2appimage icon")

# alr2appimage

`alr2appimage` is a tool to automatically create an
[AppImage](https://appimage.org/) executable from an Alire crate.

> [!NOTE]
> `alr2appimage` is an independent project; it is not
> affiliated to, nor supported by, the Alire or AppImage projects.

# How to use the tool

There are two prerequisites for your project to work with this tool:
- It has to be a crate with an `executables` field. Its first value
  has to be the main application program. Alternatively, you can
  provide a value in the command line.
- It must be installable using Alire or gprinstall, including all the
  needed resources.


`alr2appimage` will use the following command for installing it (this requires Alire 2.0):
```shell
alr install
```
Or it will run `gprinstall` inside `alr exec`, if the former fails (Alire 1.x).

If you simply run the tool inside an Alire crate, it will read the
metadata from your `alire.toml` file and create a default AppImage
from it.

The utility [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy)
is automatically downloaded using `curl` or `wget` tools. If you
cannot install one of these utilities or lack an Internet connection in the
host, just put a copy of the `linuxdeploy-your_arch.AppImage` in the
crate directory, and it will be used by `alr2appimage`.

This tool will generate a valid [Destop Entry](https://specifications.freedesktop.org/desktop-entry-spec/latest/)
for the AppImage. The following metadata will be read from your
`alire.toml` for the generation, and will be matched to the equivalent
field of the `your_crate.desktop` file:

| Alire         | Desktop    | Comment                                                                                                                                                                                                                               |
|---------------|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `name`        | `Name`     | It will also be the base name of your AppImage file.                                                                                                                                                                                  |
| `description` | `Comment`  |                                                                                                                                                                                                                                       |
| `tags`        | `Keywords` | Additionally, any of the tags matching (case-insensitively) one of the main categories in the [freedesktop.org menu specification](https://specifications.freedesktop.org/menu-spec/menu-spec-1.0.html) will be used as `Categories`. |
| `executables` | `Exec`     | The first executable is used as entrypoint of the AppImage. It can be orverriden using the `--executable your-executable-file` argument.                                                                                              |
|               | `Terminal` | This field of the desktop entry will be set to `false` by default. It can be set to `true` passing the `--terminal` argument.                                                                                                         |
|               | `Type`     | This field of the desktop entry will be set to `Application`.                                                                                                                                                                         |
|               | `Icon`     | By default, the included icon (`alr2appimage.png`) will be used. It can be orverriden using the `--icon your-icon-file` argument.                                                                                                     |

## Including resources
To include the resources, just add this section to your GPR file:
```ada
   package Install is
      for Artifacts (".") use ("share");
   end Install;
```
And include the resources in the repository in that directory, e.g. in `share/`.

If your crate has resources, it is recommended to use the `resources`
crate, or a similar mechanism, to properly load the resource files from
the installation prefix.

The next step is referencing the resources from the application.  it
is recommended to use the
[resources crate](https://github.com/alire-project/resources),
or a similar mechanism, to properly load the resource files from the installation
prefix.  `alr2appimage` is doing this for the default AppImage icon.

Another approach, if you don't want to add another dependency, is
doing the logic of getting the base directory of the executable by
yourself. This is an example of how to do this, assuming it's running
under Linux and the executable is located under `<prefix>/bin/`.

```ada
   function Application_Prefix return String is
      Self_Exe : constant String := "/proc/self/exe";
   begin
      -- Get the resources path through the directory where the program is
      -- located.
      -- In this way we allow running from an AppImage and still find the
      -- resource files.
      --
      if Ada.Directories.Exists (Self_Exe) then
         return Ada.Directories.Containing_Directory
           (Ada.Directories.Containing_Directory
              (Ada.Directories.Full_Name (Self_Exe)));
      else
         return "";
      end if;
   end Application_Prefix;
```

# Usage
```
Usage: alr2appimage [OPTIONS]...
Makes an AppImage from your Alire crate.

  -h, --help       Display this help text.
  -V, --version    Display the version of this utility.
  -t, --terminal   Set the Terminal flag of the AppImage to true, i.e. the application is for the terminal or requires to be run from a terminal.
  -i, --icon       Specify the icon file for the AppImage
  -e, --executable Specify the executable for the AppImage (without path)
```

Run inside an Alire crate to create a default AppImage of your
application.  With the corresponding arguments, you can override the
executable, provide your own icon and override the default value
(false) for the terminal field.


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

You can also download an AppImage from the releases section or use
`alr install alr2appimage` to install the utility from the Alire index.
