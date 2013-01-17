oasis = require("oasis")
darcs = require("darcs")
ci = require("ci")

ci.init()
oasis.init()
darcs.init()

ci.prependenv("PATH", "/usr/opt/godi/bin")
ci.prependenv("PATH", "/usr/opt/godi/sbin")

oasis.std_process()
darcs.create_tag(oasis.package_version())
