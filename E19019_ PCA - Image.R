
# Code Created by Onkar Ankalkope(E19019) and Gnaneeswar Adhi (E19011) #

# Change the filepath according to your laptop 

Originalpath<-"C:\\Users\\User\\Desktop\\Praxis\\R\\Original\\"
Compressedpath<-"C:\\Users\\User\\Desktop\\Praxis\\R\\Compressed\\"

#------------------------------------------------------------------------------

library(jpeg)
continue<-"Yes"
cat(c("The available images are ",list.files(Originalpath)))

while (tolower(continue)=="yes")
  
{

imagefetch<-0
while (imagefetch==0)
{
  # error  Handling if file name is entered wrongly
  tryCatch(
    {
      image.name <- readline(prompt="Enter the image name (with extension .jpg or .jpeg) which you want to compress: ")
      readpath<-paste(Originalpath,image.name,sep="")
      image<-readJPEG(readpath)
      imagefetch<-1
    },
    error=function(cond) 
    {
      cat(c("The entered Image does not exist. The available images are ",list.files(Originalpath)))
      imagefetch<-1
    })
}  

print("wait for a minute, fetching the image")

# Extractiung the RGB components separately

r<-image[,,1]
g<-image[,,2]
b<-image[,,3]

# PCA on each of the RGB components separetly
# pT = aT xT

image.r.pca <-prcomp(r,center=FALSE)
image.g.pca <-prcomp(g,center=FALSE)
image.b.pca <-prcomp(b,center=FALSE)

# Cumulative sum of variance for till the last principal component

rr<-round(cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100,2)
cat(c("cumulative variance of the first ten principal components in Red ",rr[1:10],"\n"))
gg<-round(cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100,2)
cat(c("cumulative variance of the first ten principal components in Green ",gg[1:10],"\n"))
bb<-round(cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100,2)
cat(c("cumulative variance of the first ten principal components in Blue ",bb[1:10],"\n"))

ncomp<-0
count<-0
flag<-0

getoption <- readline(prompt="Do you want to compress image by No. of principal components or by clarity. Type either number or clarity ")

if (tolower(getoption)=="number")
{
  print(paste("the number of components present in the image is",length(rr)))
  ncomp<-readline(prompt="enter the number of components you want to retain")

}

if (tolower(getoption)=="clarity")
{

  print("images with 98% or more clarity look good")
  print(paste("the minimum limit to clarity is",min(c(min(rr),min(gg),min(bb))),sep=" "))
  var.limit <- readline(prompt="Enter the percentage clarity that you need in two digits")
  
  while (flag==0)
  {
    count=count+1
    
    if(((rr[count]>=as.double(var.limit))&(gg[count]>=as.double(var.limit)))&(bb[count]>=as.double(var.limit)))
    {
      ncomp<-count
      flag=1
      print(paste("number of components selected is: First",ncomp,sep=" "))
    }}
}

# getting the compressed components
# x = p * a
R=image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
G=image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
B=image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])

# making necessary coreection with regards to bounded ness
R= ifelse(R>1,1,R)
G= ifelse(G>1,1,G)
B= ifelse(B>1,1,B)

R= ifelse(R<0,0,R)
G= ifelse(G<0,0,G)
B= ifelse(B<0,0,B)

img=array(c(R,G,B),dim=dim(image))
summary(img)

writepath<-paste(Compressedpath,image.name,sep="")
writeJPEG(img,writepath)

original.delete<-readline(prompt=" Do you want to delete the original image ? Enter Yes or No")

if(tolower(original.delete)=="yes")
{
  file.remove( paste(Originalpath,image.name,sep=""))
  print("original image deleted")
}
  
continue <- readline(prompt=" Do you want to continue with compressing any other image? Enter Yes or No ")

if(tolower(continue)!="yes")
{
  print(" Thanks for using the function!. Bye")
}

}

