# AspectL
NOTE: AspectL is deprecated, because the pointcut functionality of AspectL does not make a lot of sense in Common Lisp, and the support for dynamically scoped generic functions has been replaced with much better mechanisms in [ContextL](https://github.com/pcostanza/contextl "ContextL").

AspectL is a library that provides some experimental aspect-oriented extensions for Common Lisp / CLOS. See [an overview](https://common-lisp.net/project/closer/aspectl-overview.html "AspectL Overview") for a description of its functionality.

AspectL depends on [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP"), and is therefore only supported by the Common Lisp implementations that are supported by that library.

New in version 0.75.0:
* New version number based on semantic versioning.
* Added support for Allegro Common Lisp 8.2 & 9.0, and LispWorks 6.1.

Version 0.74 of AspectL is now available. The major news for 0.7 were as follows:
* AspectL is now part of the Closer Project. This means that it takes advantage of the Closer to MOP compatibility layer, so that it runs on more Common Lisp implementations than ever before.
* Some of the functionality was previously ported from AspectL to ContextL, mostly because it is not genuine aspect-oriented functionality. Those parts are now removed from AspectL implementation-wise in order to avoid code duplication. Instead, AspectL imports that functionality from ContextL, and exports it again for compatibility reasons. Existing AspectL code should mostly work as before.
* Likewise, the CLOS MOP wrappers are completely removed because Closer to MOP supports compatibility across different CLOS MOP implementations much better.
* Finally, the package structure is much simplified. Instead of placing every functionality in its own package, there is now the (previously already available) ASPECTL package from which everything can be imported. The previous scheme was too complicated and offered no obvious advantages.

Allegro Common Lisp 7.0 has the limitation that defmethod does not accept more than one qualifier. This means that the :override qualifier of special-method-combination can only be used for primary methods. This has been fixed in Allegro Common Lisp 8.0. That version supports the full functionality of AspectL.

Special generic functions do not work on CMU Common Lisp, and Macintosh Common Lisp since they currently don't implement the generic function invocation protocol as specified in The Art of the Metaobject Protocol.

For release notes of versions before 0.7 please see [the release notes at the old AspectL homepage](https://common-lisp.net/project/aspectl/release-notes.html "AspectL - Release Notes").
