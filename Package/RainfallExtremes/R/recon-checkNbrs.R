#check neighbours are valid for reconstruction
#
print("Reduced this function to one get.nbrs, will run twice once for ZSeries and then BSeries")
print("Dependecy - compare.sites")

check.nbrs <- function(data, metaData, nearest.nbrs, dataDir){
  #changed this keep.nbrs has different errors depending on why the site was deemed unsuitable
  if(length(nearest.nbrs)==0){return(NA)}
  keep.nbrs = sapply(nearest.nbrs, compare.sites, data = data,
                    metaData = metaData, dataDir = dataDir)
  num.keep = sum(keep.nbrs > 0, na.rm = T)
  if(num.keep > 0){
    keep.nbrs = order(keep.nbrs, decreasing = T)[1:num.keep]
    valid.nbrs = nearest.nbrs[keep.nbrs]
  }else{
    valid.nbrs = NULL
  }

  return(valid.nbrs)
}
