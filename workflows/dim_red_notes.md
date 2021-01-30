tSNE notes:

	we used barnes hut t distributed stochastic neighbor embedding. 
	Package used was Rtsne in R which provides an R wrapper to 
		the C++ implementation of Barnes-Hut t-Distributed Stochastic Neighbor Embedding.
	Frequency data was binned to 150 bins from the original ~300 frequencies, then the mean at each frequency 
	across each temperature group was computed. Data was normalised prior to embedding to a scale of 0 - 1. 
	This preserved the data distribution far better than the common scaling and centering approach used. 

	Perplexity of 5 was used. Computing tSNE on varying levels of perplexity did not show any notable differences 
	in the final clustering achieved. Generally a perplexity of 5 to 50 is recommended. 

	Theta of 0 was used to allow for computing exact tSNE. Since our data was the population means, exact tSNE could
	be computed without computing resources required being too large. 
	
	PCA was computed prior to embedding using the core stats package in R. 50 principal components were extracted
	from this transformation to further reduce the computation requirements for the final embedding. 

	Population means were used for the entire processing pipeline both to reduce the computational requirements of
	the final embedding but also to reduce much of the noise found at the population scale. 

PCA notes:
	
	we used the core stats package in R for computing PCA. A binning regime of 150 was used. All data was normalised
	to a uniform index of 0 - 1 prior to the PCA transformation to preserve the data distribution across frequencies. 

	This is in contrast to the standard approach of standardizing data to a mean of 0 and sd of 1 because our 
	frequency data exhibit a specific range relative to their frequency. This is simply a result of lower frequencies
	emitting more energy relative to higher frequencies. Thus normalising data allows us to preserve the signatures
	of all of our frequencies without exaggerating the signatures of specific frequencies. 

	Also important to note that standardizing data did not in fact produce any useful transformations/clusters, 
	whilst normalising them did allow us to converge on clusters that represent the global structure of the data
	in high dimensional space. 













