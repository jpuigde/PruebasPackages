install.packages("_packages/sshtools",repos=NULL, type="source")
install.packages("_packages/astertools",repos=NULL, type="source")
require(astertools)
require(sshtools)
help.allmanuals()

sshtools::help.sshtools()


con = ssh.shell()
ssh.open(con, server, port, user, pass)
