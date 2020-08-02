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

inline unsigned int sample_n(unsigned int n) {
  return (floor(unif_rand() * n));
}

// [[Rcpp::export]]
std::vector< unsigned int > permute(
  const std::vector< std::vector< unsigned int > > & candidates
) {

  std::vector< unsigned int > idx(candidates.size());
  std::iota(idx.begin(), idx.end(), 0u);
  std::vector< bool > picked(idx.size(), false);
  
  std::vector< unsigned int > res(idx);
  
  // Permuting until idx is of size 0
  int nleft = idx.size();
  while (nleft > 0) {
    
    // Selecting from idx
    unsigned int i = sample_n(nleft);
    
    // Was it picked as j in a previous run?
    if (picked[idx[i]]) {
      idx[i] = idx[--nleft];
      continue;
    }
    
    // If empty, then remove and go to the next
    unsigned int j;
    if (candidates[idx[i]].size() == 0u) {
      
      picked[idx[i]] = true;
      idx[i]         = idx[--nleft];
      continue;
      
    } else if (candidates[idx[i]].size() == 1u) {
      
      j = candidates[idx[i]][0u];
      
      // Was it picked before?
      if (picked[j]) {
        
        picked[idx[i]] = true;
        idx[i]         = idx[--nleft];
        continue;
        
      }
      
    } else {
      
      // Temp copy that can be discarded
      std::vector< unsigned int > tmpc(candidates[idx[i]]);
      unsigned int nleft_j = tmpc.size();
      
      bool pending = true;
      while (pending) {
        
        j = sample_n(nleft_j);
        if (picked[tmpc[j]])
          tmpc[j] = tmpc[--nleft_j];
        else { // Case in which 
          
          pending = false;
          j       = tmpc[j];
          break;
          
        }
        
        if (nleft_j == 0u)
          break;
          
      }
      
      // Was not able to find anything
      if (pending) {
        
        picked[idx[i]] = true;
        idx[i]         = idx[--nleft];
        continue;
        
      }
    }
    
    // Applying the permutation
    res[idx[i]] = j;
    res[j]      = idx[i];
    
    // "Removing" from the list
    picked[idx[i]] = true;
    picked[j]      = true;
    idx[i]         = idx[--nleft];
    
  }
  
  return res;
  
}


/***R
library(data.table)

# Reading the data and identifying individuals and their first shooting event
njforce <- data.table::fread("data-raw/njforce_200210.csv")
reports <- subset(
  njforce,
  select = c(
    date, officerid, firearm_discharged, firearm_pointed, incidentid,
    officer_male, officer_nyears, officer_race, officer_rank, town,
    officer_po, officer_sleo, nsubjects, Incident_type
  ))
reports[, date := as.Date(date, format = "%m/%d/%Y")]

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

# Checking whether we get the same result each time
set.seed(134)
ans <- replicate(2000, permute(cand), simplify = FALSE)


microbenchmark::microbenchmark(
  permute(cand), times = 500
)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# permute(cand) 2.586945 2.805324 3.453647 2.934757 3.244724 30.28904   500

# Checking unique values match
unique(sapply(ans, function(i) length(unique(i))))
 
# Checking frequencies (is it uniform?)
ans <- do.call(cbind, ans)
ans_freq <- apply(t(ans), 2, table)

# Checking that we always got what it was intended
ans_cand <- apply(ans, 1, unique)
ans_cand <- lapply(ans_cand, sort)
ans_cand <- sapply(1:length(ans_cand), function(i) {
  all(ans_cand[[i]] %in% c(cand[[i]], i - 1))
})
table(ans_cand) # All should be true!

*/