#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector< std::vector<int> > find_candidates(
  const IntegerVector & dates,
  const IntegerVector & officer_id,
  const IntegerVector & incident_id,
  const IntegerMatrix & ematch,
  int window
) {
  

  std::vector< std::vector< int > > candidates(dates.size());
  
  for (auto i = 0; i < dates.size(); ++i) {
    
    for (auto j = 0; j < i; ++j) {
      
      if ((officer_id[i] == officer_id[j]) || (incident_id[i] == incident_id[j]))
        continue;
      
      if (abs(dates[i] - dates[j]) <= window) {
        
        // Using Rcpp sugar to compare two vectors
        unsigned int ndiff = 0u;
        for (auto k = 0u; k < ematch.ncol(); ++k)
          if (ematch(i, k) != ematch(j, k))
            ndiff++;
        
        if (ndiff > 0u)
          continue;
        
        candidates[i].push_back(j);
        candidates[j].push_back(i);
      }
        
    }
  }
  
  return candidates;
}

// [[Rcpp::export]]
std::vector< unsigned int > permute(
  const std::vector< std::vector< unsigned int > > & candidates,
  const std::vector< unsigned int > & officer_id
) {
  
  std::vector< unsigned int > officer_id_copy(officer_id);
  
  std::vector< unsigned int > idx(officer_id.size());
  std::iota(idx.begin(), idx.end(), 0u);
  std::vector< bool > picked(idx.size(), true);
  
  // Permuting until idx is of size 0
  while (idx.size() > 0) {
    
    // Selecting from idx
    unsigned int i = floor(unif_rand() * idx.size());
    
    // If empty, then remove and go to the next
    if (candidates[i].size() == 0u) {
      idx.erase(idx.begin() + i);
    }
    
    // Picking one candidate to be removed
    unsigned int j = floor(unif_rand() * candidates[i].size());
    
    
  }
  
  
  return officer_id_copy;
  
}


/***R
cand <- find_candidates(
  dates       = as.integer(reports$date),
  officer_id  = as.integer(reports$officerid),
  incident_id = as.integer(reports$incidentid),
  ematch = cbind(
    as.integer(reports$officer_male),
    as.integer(as.factor(reports$town))
    ),
  window = 15  
  )

hist(sapply(cand, length) - 1)
*/