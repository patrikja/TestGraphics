2016-11-06: Trying out FieldTrip

Failed to build due to old dependencies.

Next: try to download the FieldTrip sources and tweak the dependencies.
```shell
wget http://hackage.haskell.org/package/FieldTrip-0.2.7/FieldTrip-0.2.7.tar.gz
tar zxf FieldTrip-0.2.7.tar.gz
cabal unpack graphicsFormats
cd FieldTrip-0.2.7
```

2016-11-13: Experimenting with minor updates to the cabal files to
make the libraries build. Removed the dependency on haskell98 from
graphicsFormats. Current state:

```shell
cabal install graphicsFormats-0.1/ FieldTrip-0.2.7/
...
src/Graphics/FieldTrip/Vector2.hs:29:49:
    Module
    ‘Graphics.Rendering.OpenGL.GL.CoordTrans’
    does not export
    ‘Vector2(..)’

```
Which indicates that the interface of OpenGL has changed. TODO.


----


(Also note that 3D graphics is more than we need.)
