K-Means Clustering
==================

K-means is as simple, intuitive, and fast algorithm for clustering data.  In this sample, I apply it to results from a gas-liquid chromatograph (GC) run on samples of (methylated) fatty acids (FA).  

## Using K-means to Identify Fatty Acid Composition in Animals
My goal in this project was to take fat samples from a number of animal species and determine the proportions of different FAs present.  This was part of a larger project <a href="http://janimscitechnol.biomedcentral.com/articles/10.1186/2055-0391-56-16">(published here)</a> that inferred predator diet based on the FA profile of it and its candidate prey species.

In short, GC is used to identify different molecules in a liquid compound.  The compound is slowly heated, and as different molecules evaporate off, they move through a long tube, which may have other properties (such as charge) that further interact with the molecules on the basis of their physical or chemical properties (e.g., polarity).  For all molecules passing through the tube, the retention time is recorded, and the GC outputs those times for the user.  In general, all identical molecules will have approximately the same retention time, but there will be some random variation.  Standard samples with known composition can be obtained and run through the GC, and the results of samples with unknown composition are compared to these in order to identify their composition.  Most GCs will automatically identify any molecules that are the best match for the knowns in the standard, but other molecules will be left unidentified.

Difficulties can arise in a number of ways:
*Typically there are more FAs present in the sample than in the standards, so either many standards must be obtained, or the researcher must be content with having some FAs remaining unidentified.
*If two or more FAs have ranges of retention times that overlap, there is no way to identify which molecule is which with absolute certainty, on the basis of the GC alone.
*Impurities will typically be present in the sample, causing the GC to identify a molecule which is not a FA at all.

The goal of the k-means clustering in this case, then, is to look at the retention times from all of our samples (each sample being a fat sample from a single animal), and cluster them based on their retention times, where each cluster, in principle, represents a unique fatty acid. 



## The R Script
The raw data that went into this analysis may be found in the <a href="http://www.phillips-research.com/ml/FatCalib.csv">here</a>.

We wish to cluster all molecules in all samples according to their retention time.
The overall approach taken here is as follows.
1. For all FA with known identities, determine their median retention time across all samples, and use the median values to calibrate samples such that the calibrated retention times match exactly for the known FAs.
2. Use the retention times of the known FAs as one set of centroids that will only be allowed to vary slightly in value.
3. Because there are more FAs in the samples than the known ones, we add additional centroids, with randomly intialized values (uniform over the range of observed retention times).  There are two approaches that might be taken here: 
	+ Optimize to find the number of centroids that best explains the variation
	+ Set an fixed number of additional cetroids.
Because the optimization approach led to predictions of many fewer FAs than we knew to be present based on existing literature, we ended up going with a fixed number, also based on the literature.  
4. Using the set of centroids, run the k-means algorithm and determine the amount of variance explained by the groupings (sum of squares between clusters / total sum of squares).
5. Randomly reinitialize centroids and perform k-means again.  If new clusters do a better job of explaining the variance, save this group of clusters instead.
6. Repeat for an arbitrary number of iterations, saving the best clusters any time they outperform the existing saved set of clusters.


```{r initialize}
#===========================================================================#
#																			#
# finalClust.r                                            					#
#                                                        					#
# This script takes all the original GC mean retention times (MRT), scales	#
# them to calibrate, then does a cluster analysis using clusters with known	#
# MRT used to center a known number of clusters. 							#
#																			#
# Returns the assignment of each MRT to a specific cluster                 	#
#                                                         					#
# Damian Satterthwaite-Phillips <damiansp@gmail.com>      					#
# 19 Nov 2012                                            			 		#
#                                                         					#
#===========================================================================#

# Start with a clear workspace
rm(list=ls())
library(MASS)

# Read in data
fat = read.csv('~/Desktop/PRA/WebsiteRoot/ML/FatCalib.csv')

# Save data dimensions in variables
ROWS = dim(fat)[1]
COLS = dim(fat)[2]

# label rows with known FAs
known = c(3, 12, 19, 25, 31, 39, 49, 59, 71 ,75, 86, 89, 95, 100, 104, 111, 
		  114, 121, 124, 126, 127, 132, 134, 137, 142, 145, 149, 150, 152, 
		  153)	
# Check to make sure rows were correctly identified:
fat$FA[known]
```
The "knowns" here are based on standards.  In the data, known FAs are named as "x_y(nz)", e.g., "10_0" or "20_4n6", where x represents the number of carbon atoms in the chain, y indicates the number of C=C double bonds present, and z indicates the position of the endmost double bond.  For more information about FA composition and nomenclature see <a href="https://en.wikipedia.org/wiki/Fatty_acid">the Wikipedia page</a>.


```{r init2}
fat$FA[-known]
names(fat)[1:10]
```
Unknowns were simply named 'a', 'b', 'c', ... where all 'a's had retention times less than the first known, all 'b's had retention times between the first and second known, and so forth.  The column names in the data are: "FA" to identify the different FAs present, then a columns representing individual samples.  These are labeled with  one or more letter identifying the taxonomic group of the sample, and a number to identify the individual animal (e.g. "O1" refers to "Otter no. 1",  "CV", and "CP" are crafish species, "MRP", "MWP", "MFM", etc. are all mussel species, and so forth). The final designation for each column is and underscore followed by an "MRT", "H" or "A", which stand for "Mean Retention Time" (e.g., of all molecules of a given type for the sample), and "Height", and "Area" of the curve output by the GC.  Here we will only be concerned with MRT.


```{r init3}
mrts = seq(2, COLS - 2, 3)
#hs = seq(3, COLS - 1, 3)
#as = seq(4, COLS, 3)

# Again check to make sure the correct columns were identified (limited to 10 here 
# for brevity)
names(fat[, mrts])[1:10]
```

The next big of code is the calibration, to align the values of the knowns
```{r calibrate}
# Set mrt value to calibrate knowns to median value of first group 
# [cols 1:313])
# For each iteration of the following for loop, all MRTs for the ith known FA 
# are gathered (from all samples--0 values omitted), and stores the median value 
# for each in calibs.  (Because the distribution of MRTs for a given FA are 
# typically non-normal, median is preferred to mean to find the central tendency.)
calibs = numeric(length(known))
for (i in 1:length(calibs)) {
	mrtsForKnown = 
		fat[known[i], mrts[mrts <= 313]][
			# omit 0 MRT values
			fat[known[i], mrts[mrts <= 313]] > 0
		]
	calibs[i] = median(mrtsForKnown, na.rm = T)
}

names(calibs) = fat$FA[known]
# Plot the calibrated values as a sanity check
plot(calibs, type = 'h', 
	 ylab = 'Mean Retention Time (Median Values)', xlab = 'Fatty Acid',
	 xaxt = 'n')
axis(1, labels = names(calibs), 
	 at = 1:length(calibs), las = 2, cex.axis = 0.5)
```

The graph looks good. In general the larger the FA molecule, the longer its retention time (i.e., the more slowly it moves through the GC), which is as expected.  

```{r calibrate2}
# Scale according to calibs: Start at highest MRT scale everything earlier; move 
# to next highest, rescale everything earlier, etc.
# In short, this for loop takes all the known MRTs and recalibrates them to make 
# them all share the median value
for (i in length(known):1) {
	for (j in mrts){
		# Make sure there is a nonzero value at the known
		# If not skip to next
		if (fat[known[i], j] > 0) {
			fat[1:known[i], j] = (fat[1:known[i], j] * calibs[i]) / 
				max(fat[1:known[i], j])
		}
	}
}

# Save data
# write.csv(fat, '~/Desktop/FAT Working/fatScaled.csv')


# Plot the (now calibrated) MRTs for the knowns
plot(as.vector(as.matrix(fat[known, mrts][fat[known, mrts] > 0])), 
	 xlim = c(1, 9000), ylim = c(2, 46), pch = 16, col = rgb(0, 0, 0, 0.1),
	 xlab = 'Identified Molecules (All Samples)', ylab = 'Mean Retention Time')
# Add in the unknowns
points(as.vector(as.matrix(fat[-known, mrts][fat[-known, mrts] > 0])), 
	   pch = 16, col = rgb(1, 0, 0, 0.1))
# Add lines showing the calibrated MRT for the known FAs
abline(h = calibs)
legend('topright', pch = c(16, 16, NA), lty = c(NA, NA, 1), col = 1:2, 
	   title = 'FA Identity', 
	   legend = c('known', 'unknown', 'calibrated MRT\nfor known FAs'), 
	   bg = rgb(1, 1, 1, 0.9))
```

The <em>x</em>-axis on this graph has no special significance. It is simply all of the molecules in all the samples, ordered by their retention times, one species after the next.

The horizontal lines show the calibrated retention times for the known FAs, and the black points (the calibrated knowns), now fall precisely at those values.  The goal now is to identify all of the (red) unknowns, which may have an identity equal to one of the knowns, or may be an unidentified FA, not present in our standard.

The following code performs the k-means clustering, and iterates an arbitrary number of times, returning the best cluster found over all iterations.

```{r cluter}
# Cluster the data, with total number of centers specified 
#  and (optionally) any pre-specified centers.  
#  @param data: vector of data points to be clustered
#  @param tot.centers: the total number of desired clusters
#  @param minVal: minimum possible value for centroids
#  @param maxVal: maximum possible value for centroids
#  @param spec.centers: a vector of the centroids for any
#	      pre-specified centers
#  @param iter.per: number of iterations for each run of
#	      the k-means algorithm
#  @param tot.iter: total number of individual runs of 
#	      k-means, randomizing initial positions of any
#	      non-specified centroids.
#  @return: a kmeans object
bestClust = function(data, tot.centers, tot.iter, minVal, maxVal, 
					  spec.centers = c(), iter.per = 10) {
	k.out = 0				  	
	class(k.out) = 'try-error'					  	

	while(class(k.out) == 'try-error') {
		rand.centers = runif(tot.centers - length(spec.centers), minVal, maxVal)
		k.out = try(kmeans(data, c(spec.centers, rand.centers), iter.per), 
					silent = T)
	}
	
	best.eta = k.out$betweenss / k.out$totss
	best.clust = k.out
	
	# repeat and save if better model:
	for (i in 1:tot.iter) {
		this.out = 0
		class(this.out) = 'try-error'
		
		while(class(this.out) == 'try-error') {
			rand.centers = runif(tot.centers - length(spec.centers), minVal, 
								 maxVal)
			this.out = try(kmeans(data, c(spec.centers, rand.centers), iter.per),
						   silent = T)
		}
		
		this.eta = this.out$betweenss / this.out$totss
		if (this.eta > best.eta) {
			best.eta = this.eta
			best.clust = this.out
		}
	}

	plot(data, col = this.out$cluster, pch = 16, cex = 0.7)
	abline(h = spec.centers)
	return (this.out)
}

# Apply: Find best clusters assuming 45 centers
set.seed(11)
clust_60FA = bestClust(
	as.vector(as.matrix(fat[,mrts][fat[,mrts] > 0])), 
	tot.centers = 60, tot.iter = 1000, 
	minVal = 3.47, maxVal = 40.26, 
	spec.centers = jitter(calibs)
)
```

The output graph is color-coded to show which molecules belong with which cluster.  
The number of clusters used here was 60, based on an approximate number known to be present from the literature, though the number is necessarily somewhat arbitrary.  As can be seen, it appears likely that some distinct molecules have been clustered together.  For our purposes, this was not especially problematic as we went on to use these clusters in a principal components analysis, and combining clusters essentially just meant a loss of information.  But, we have benefitted by obtaining a systematic approach to identifying the FAs that minimized the within-cluster variance.

We can now stor these results for further use.
```{r save}
# Create a new data.frame that gives just the (scaled) retention time and the 
# inferred identity for each molecule
assignments45FA = data.frame(
	scaledMRT = as.vector(as.matrix(fat[,mrts][fat[,mrts] > 0])),
	cluster = clust_45FA$cluster
)

head(assignments45FA)

# write.csv(assignments45FA, '~/Desktop/FAT Working/assignments45FA.csv')
```
