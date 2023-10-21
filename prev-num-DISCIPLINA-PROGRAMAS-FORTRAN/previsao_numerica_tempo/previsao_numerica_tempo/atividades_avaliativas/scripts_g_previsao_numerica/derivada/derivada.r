der2.frente <- function(psi, dx)
{
ddpsiloc <- NA
im <- length(psi)
for( i in 1:im)
{
if(i != im)
{
ddpsi <- 2*((dx^3-dx^2)/(dx^5))*(psi[i+1]-psi[i])
}
else
{
ddpsi <- 2*((dx^3-dx^2)/(dx^5))*(psi[1]-psi[i])
}
ddpsiloc <- c(ddpsiloc, ddpsi)
}
return(ddpsiloc)
}