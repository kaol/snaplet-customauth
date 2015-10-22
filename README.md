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
main differences are that AuthManager takes two type parameters, one
of which is the custom user type, and that you'll need to write your
own backend.  It may have arbitrary application specific data attached
to the user data, it just has to implement the UserData type class,
which exposes a few common attributes that the authentication
management code needs and uses.

Also, the only session variable storage and retireval in this model is
done against the database.  I don't plan on using the cookie and
symmetric site key based Snap.Snaplet.Session along with this code.

CustomAuth doesn't offer any ready backends, but lets you implement
your own with the IAuthBackend type implementation.  I may yet rename
that type class.

## Current status

Very much work in progress.  This version still includes the web site
code that I'm working on along with this.  That part pretty much looks
like Snap default template.  Look at src/Backend.hs to see the main
idea of how I wanted to use an authentication framework.  I'll remove
that code if/when this code moves to Hackage.

The included cabal file has plenty of dependencies that
snaplet-customauth won't have.  I opted to use Hasql for my database
code.  One thing that needs to change is to use snaplet-hasql and not
pass a plain hasql pool to my authentication library.

Running this code requires getting Piperka's schema.  My plan is to
work on snaplet-customauth along with the new Piperka backend for now.
