#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double square( double x ) {
	return x*x;
}

// [[Rcpp::export]]
double rnd( double x ) {
	return round(x);
}

// [[Rcpp::export]]
double diffs(NumericVector bill, NumericVector mc) {
	NumericVector dist = mc - bill;
	NumericVector absDist = abs(dist);
	NumericVector powDist = sapply(absDist, square);
	double distSum = sum(powDist);
	return distSum;
}

// [[Rcpp::export]]
NumericVector adjud(NumericVector propDiffs, NumericVector sqDiffs) {
	NumericVector billDiff = propDiffs - sqDiffs;
	NumericVector voteProbs = pnorm(billDiff);
	NumericVector votes = sapply(voteProbs, rnd);
	return votes;
}

// [[Rcpp::export]]
Rcpp::NumericVector rcFn(NumericMatrix props, NumericMatrix sqs, NumericMatrix mcs) {
	
	/* Pull dimensions of matrices */  
	int nProp = props.nrow(), nMC = mcs.nrow();

	/* Create a blank matrix to populate */
	NumericMatrix out(nMC, nProp);

	/* Loop through each MC */
	for (int i = 0; i < nMC; i++) {
		
		/* Pull the vector representing the MC's ideal point */
		NumericVector mc = mcs(i, _);

		/* Empty vectors for sqs and proposals */
		NumericVector propDiffs(nProp);
		NumericVector sqDiffs(nProp);

		/* Difference between MC ideal point and each sq/proposal */ 
		for (int j = 0; j < nProp; j++) {
			NumericVector prop = props(j, _);
			NumericVector sq = sqs(j, _);

			propDiffs[j] = diffs(prop, mc);
			sqDiffs[j] = diffs(sq, mc);
		}

		/* Adjudicate between proposals and SQs */
		NumericVector votes = adjud(propDiffs, sqDiffs);

		/* Populate matrix of votes */
		out(i, _) = votes;

	}

	/* Return the matrix of votes */ 
	return out;

}