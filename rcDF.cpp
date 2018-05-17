#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double square( double x ){
	return x*x;
}

// [[Rcpp::export]]
double rnd( double x ){
	return round(x);
}

// [[Rcpp::export]]
NumericMatrix voteFn(NumericMatrix props, NumericMatrix sqs, NumericMatrix mcs) {
	
	/* Pull dimensions of matrices for looping, create a blank matrix to populate */
	int nrowProp = props.nrow(), nrowMC = mcs.nrow(), ncol = props.ncol();
	NumericMatrix out(nrowProp, ncol);

	/* Loop through each MC */
	for (int i = 0; i < nrowMC; i++) {
		
		/* Pull the vector representing the MC's ideal point */
		Rcpp::NumericVector mc = mcs(i, _);

		/* Initialize vectors for the MC's distance to the proposal and SQ points */
		NumericVector propDists(ncol);
		NumericVector sqDists(ncol);

		/* Loop through the proposal and SQ ideal points */
		for (int j = 0; j < nrowProp; j++) {

			/* Pull vector representing ideal points */
			Rcpp::NumericVector prop = props(j, _);
			Rcpp::NumericVector sq = sqs(j, _);

			/* Take difference between MC and bill ideal points */
			Rcpp::NumericVector pDist = abs(mc - prop);
			Rcpp::NumericVector sDist = abs(mc - sq);

			/* Square the differences */
			Rcpp::NumericVector propDist = sapply( pDist, square );
			Rcpp::NumericVector sqDist = sapply( sDist, square );


			/* Sum elements */
			double propDistSum = sum(propDist);
			double sqDistSum = sum(sqDist);

			/* Populate distances vector */
			propDists[j] = propDistSum;
			sqDists[j] = sqDistSum;
		}

		/* Adjudicate between proposals and SQs */
		NumericVector diff = propDists - sqDists;
		NumericVector voteProbs = pnorm(diff);
		Rcpp::NumericVector votes = sapply(voteProbs, rnd);

		/* Populate matrix of votes */
		out(i, _) = votes;

	}

	/* Return the matrix of votes */ 
	return out;

}