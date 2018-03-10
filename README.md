# snaplet-customauth

Snap's own Snap.Snaplet.Auth implementation was too much of a ready
made solution for my taste.  I didn't want anything that would start
with creating its own users table, but use the existing one from my
existing web site's database instead.  Also, I enjoy a more database
oriented approach as far as session and user management goes.  I don't
want my backend code to worry about things like password hashing or
salt, but just pass the plaintext password to the database and let the
procedure do everything.

## Design

This code has been adapted from Snap's own authentication code.  The
main differences are that AuthManager takes four type parameters, one
of which is the custom user type, and that you'll need to write your
own backend.  It may have arbitrary application specific data attached
to the user data, it just has to implement the UserData type class,
which exposes a few common attributes that the authentication
management code needs and uses.

Also, the only session variable storage and retireval in this model is
done against the database.  It doesn't use Snap.Snaplet.Session.

CustomAuth doesn't offer any ready backends, but lets you implement
your own with the IAuthBackend type implementation.

This code was written for piperka.net though I hope it would be useful
for other people.

## How to use this library

A tutorial and Haddock documentation are still missing.
[piperka.net](https://gitlab.com/kaol/piperka) is the library's
primary user.
