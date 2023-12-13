#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# Along with ghis gnome-keyring-daemon --start here,
# Start the gnome-keyring-daemon from /etc/pam.d/login or /etc/pam.d/lighdm. I'm not sure which.
# I think /login is for unlocking it in terminal-based logins and lightdm for greeter-based login?
# Add auth optional pam_gnome_keyring.so at the end of the auth section and session optional pam_gnome_keyring.so auto_start at the end of the session section.
# https://wiki.archlinux.org/title/GNOME/Keyring#PAM_step
# Using the keyring

# The PAM module pam_gnome_keyring.so initialises GNOME Keyring partially, unlocking the default login keyring in the process. It should be followed by a call to gnome-keyring-daemon with the --start option to complete initialisation and to set environment variables.
# PAM step
# Note: To use automatic unlocking without automatic login, the password for the user account should be the same as the default keyring. See #Automatically change keyring password with user password.
# Tip:

#     To use automatic unlocking with automatic login, you can set a blank password for the default keyring. Note that the contents of the keyring are stored unencrypted in this case.
#     Alternatively, if using GDM and LUKS, GDM can unlock your keyring if it matches your LUKS password. For this to work, you need to use the systemd init in your mkinitcpio.conf as well as the appropriate kernel parameters. See [1] for more details.
#     Skipping the PAM step works, because the next step will initialise the daemon when one is not running already; however, the default keyring is not unlocked in this case. More details are available at [2].

# When using a display manager, the keyring works out of the box for most cases. GDM, LightDM, LXDM, and SDDM already have the necessary PAM configuration. For a display manager that does not automatically unlock the keyring edit the appropriate file instead of /etc/pam.d/login as mentioned below.

# When using console-based login, edit /etc/pam.d/login:

# Add auth optional pam_gnome_keyring.so at the end of the auth section and session optional pam_gnome_keyring.so auto_start at the end of the session section.

# /etc/pam.d/login

# #%PAM-1.0

# auth       required     pam_securetty.so
# auth       requisite    pam_nologin.so
# auth       include      system-local-login
# auth       optional     pam_gnome_keyring.so
# account    include      system-local-login
# session    include      system-local-login
# session    optional     pam_gnome_keyring.so auto_start

# If you are using GNOME, Unity, Cinnamon, or MATE, you are done. The initialisation is completed and environment variables are set automatically.
# --start step

# If you are not using GNOME, Unity, Mate, or Cinnamon as your desktop environment, initialisation will not complete automatically. You can fix this using various methods:
# Shell

# Add the following to your ~/.bash_profile, ~/.zshenv, or similar:

# ~/.bash_profile

# if [ -n "$DESKTOP_SESSION" ];then
#     eval $(gnome-keyring-daemon --start)
#     export SSH_AUTH_SOCK
# fi

# eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
# export SSH_AUTH_SOCK
# eval "$(/usr/bin/gnome-keyring-daemon -s)"
# export SSH_AUTH_SOCK

# For scrolling in Nyxt and for HiDPI in Nyxt
export GDK_CORE_DEVICE_EVENTS=1
#export GDK_SCALE=2
#export GDK_DPI_SCALE=0.5
export GDK_SCALE=1
export GDK_DPI_SCALE=1

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# Default here to xfce4 since it probably came with the Linux distro
# and you'll have it before installing StumpWM. But you're greeter will
# use a .desktop file to launch this with stumpwm as an argument.
SESSION=${1:-xfce4-session}

# urxvt needs access to custom font
xset +fp /home/eihli/.local/share/fonts
xset -b # quiet the terminal beeping

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

get_session(){
    local dbus_args=(--sh-syntax --exit-with-session)
    case $1 in
        awesome) dbus_args+=(awesome) ;;
        bspwm) dbus_args+=(bspwm-session) ;;
        budgie) dbus_args+=(budgie-desktop) ;;
        cinnamon) dbus_args+=(cinnamon-session) ;;
        deepin) dbus_args+=(startdde) ;;
        enlightenment) dbus_args+=(enlightenment_start) ;;
        fluxbox) dbus_args+=(startfluxbox) ;;
        gnome) dbus_args+=(gnome-session) ;;
        i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
        jwm) dbus_args+=(jwm) ;;
        kde) dbus_args+=(startplasma-x11) ;;
        lxde) dbus_args+=(startlxde) ;;
        lxqt) dbus_args+=(lxqt-session) ;;
        mate) dbus_args+=(mate-session) ;;
        xfce) dbus_args+=(xfce4-session) ;;
        openbox) dbus_args+=(openbox-session) ;;
        stumpwm) dbus_args+=(stumpwm) ;;
        *) dbus_args+=($DEFAULT_SESSION) ;;
    esac
    # This if statement is here because of a problem I was running into where
    # org.freedesktop.secrets was not launching. It was as if gnome-keyring-daemon
    # thought org.freedesktop.secrets was available, so it wouldn't try to launch it,
    # but then when it tried to communicate with it, it would time out. So it took
    # a long time to do something like bring up the pinentry dialog.
    #
    # I noticed DBUS_SESSION_BUS_ADDRESS was being set to something like unix:abstract:asx87oas8a5...
    # And I think lightdm launches a dbus session before this, so launching it again here
    # was conflicting?
    #
    # Not entirely sure, but this fixed the issue and I don't have time to continue for
    # the sake of understanding.
    ## test for an existing bus daemon, just to be safe
    if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
        ## if not found, launch a new one
        eval `dbus-launch ${dbus_args[*]}`
        echo "D-Bus per-session daemon address is new and set to: $DBUS_SESSION_BUS_ADDRESS"
    else
        ${dbus_args[-1]}
        echo "D-Bus per-session daemon address was already set to: $DBUS_SESSION_BUS_ADDRESS"
    fi
}

get_session $1
