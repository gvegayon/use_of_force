#include <Rcpp.h>
using namespace Rcpp;

//' Find permutation candidates for each row
//' @param dates Vector of dates
//' @param unit_id Integer vector of individuals' ids (to avoid matching with
//' self).
//' @param incident_id Integer vector of the corresponding incident (to avoid
//' matching to the same incident).
//' @param ematch Integer matrix of variables over which exact match is desired.
//' @param window Integer scalar. Number of units of time to consider when
//' identifying candidates (see details)
//' @details
//' For each row `i` in the data, a row `j` is a possible permutation candidate if:
//' - `abs(date[i] - date[j]) <= window`,
//' - `unit_id` are different,
//' - `incident_id` are different, and
//' - `all(ematch[i, ] == ematch[j,])`
//' @returns
//' A list of integer vectors (starting from zero) indicating the position of
//' the potential permutation.
//' @export
//' @useDynLib njforce, .registration = TRUE
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
std::vector< std::vector<int> > find_candidates(
    const IntegerVector & dates,
    const IntegerVector & unit_id,
    const IntegerVector & incident_id,
    const IntegerMatrix & ematch,
    int window
) {


  std::vector< std::vector< int > > candidates(dates.size());

  for (auto i = 0; i < dates.size(); ++i) {

    for (auto j = 0; j < i; ++j) {

      if ((unit_id[i] == unit_id[j]) || (incident_id[i] == incident_id[j]))
        continue;

      if (abs(dates[i] - dates[j]) <= window) {

        // Using Rcpp sugar to compare two vectors
        unsigned int ndiff = 0u;
        for (int k = 0u; k < ematch.ncol(); ++k) {
          if (ematch(i, k) != ematch(j, k))
            ndiff++;
        }

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

//' Random permutation of the data as a function of `find_candidates`
//' @param candidates An integer list as that resulting from [find_candidates()].
//' @returns
//' An integer vector (indexed from 0) with the permuted version of the data.
//' @export
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
