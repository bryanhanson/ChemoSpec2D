#'
#' Traverse a Dendrogram from the Leaves Upward & Create a List of Leaves Below Each Node
#'
#' The merge matrix in a hclust result contains the order in which leaves were combined
#' into nodes, working from the bottom upward.  This function decodes the internal notation
#' described in \code{?hclust} into actual leaf numbers.  The first column in the matrix
#' is called (here) \code{Ref} and the second column \code{Mask}.  A list of nodes is
#' returned, with each node containing two lists of leaves, one for \code{Ref} and one
#' for \code{Mask}.  Each leaf list is a vector integers giving the leaf numbers that
#' were combined at that particular node. This amounts to a Guide Tree.  See the example.
#'
#' @export
#' @noRd
#'
#' @examples
#' demo <- hclust(dist(iris[1:10,1:4]))
#' plot(demo)
#' guide <- .getAlignOrder(demo)
#' guide
#' 
.getAlignOrder <- function(hc) {
  
  hcm <- hc$merge
  
  # Create a list to hold the results
  nn <- nrow(hcm) # number of nodes
  leafList <- vector("list", 2)
  leafList[[1]] <- NA_integer_
  leafList[[2]] <- NA_integer_
  names(leafList) <- c("Ref", "Mask")
  nodeList <- vector("list", nn)
  for (i in 1:length(nodeList)) nodeList[[i]] <- leafList
  names(nodeList) <- paste("Node", 1:nn, sep = "_")
  
  for (i in 1:nn) { # work through the matix of nodes
  	aRef <- hcm[i,1]
  	aMask <- hcm[i,2]
  	idx <- 0L
  	
  	while(any(aRef > 0)) { # process aRef
  	  idx <- idx + 1
  	  pos <- which(aRef > 0)
  	  chk <- aRef[pos] # chk contains the earlier rows we need to incorporate
  	  aRef <- aRef[-pos] # remove the (+) values which are references to earlier rows
  	  aRef <- c(aRef, hcm[chk,]) # append the earlier rows
  	  } # end of while loop for aRef
  
    nodeList[[i]][["Ref"]] <- sort(abs(aRef))
  	  
  	while(any(aMask > 0)) { # process aMask
  	  idx <- idx + 1
  	  pos <- which(aMask > 0)
  	  chk <- aMask[pos] # chk contains the earlier rows we need to incorporate
  	  aMask <- aMask[-pos] # remove the (+) values which are references to earlier rows
  	  aMask <- c(aMask, hcm[chk,]) # append the earlier rows
  	  } # end of while loop for aMask
  
    nodeList[[i]][["Mask"]] <- sort(abs(aMask))

  	} # end of for loop
  	
  nodeList
}

