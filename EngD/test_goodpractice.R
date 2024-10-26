# Load the goodpractice package
library(goodpractice)

# Path to the example package
# Run goodpractice on your package directory
gp_results <- goodpractice::gp("..")

# Print the results
print(gp_results)
