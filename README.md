# afile

## Read this first

This doesn't work and it's a proof of concept for now. I have
something else in mind, but due to the lack of code generation
capabilities of Ada, I have to think of work arounds (though I
believe there exists a generator tool, provided byt he GNAT
extensions). More on this in the future.

## Description

Afile is `file` written in Ada. It just looks at file headers in the
hopes of detecting signatures (magic numbers). The current
implementation is a little naive and ugly, but I plan to reiterate
over this on the long run.

## Build

   make

## Example usage

```nocode
   afile /tmp/examples/*

   /tmp/examples/some.gif: gif picture (89a)
   /tmp/examples/some.png: PNG picture
   /tmp/examples/some.ogg: Ogg audio
   /tmp/examples/another.gif: gif picture (87a)
```

That's pretty much it at the moment.

# LICENSE

See LICENSE
