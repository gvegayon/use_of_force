#include <Rcpp.h>
#include <map>
using namespace Rcpp;

class PersonDyn {

public:

  // Checking if colleague has used it or not
  std::map<int, bool> colleagues;

  // Rolling number of exposure
  int exp = 0;

  bool has_used = false;

  PersonDyn() {};

};

class Event {
public:
  std::vector< int > loc;
  Event(int i) : loc({i}) {};
  Event() {};
};

// Data must be sorted by individual
// [[Rcpp::export]]
IntegerVector exposure(
  const IntegerVector & id_indiv,
  const IntegerVector & id_events,
  const IntegerVector & time,
  const IntegerVector & actions
) {

  int N = id_indiv.size();
  IntegerVector ans(N);
  std::map<int,Event> events;

  // First iteration: Pairing individuals with events
  int i = 0;
  while (i < N)
  {

    // Keeping the event id.
    int tmp_event_id = id_events[i];

    // Does not exists, so we start moving ahead
    if (events.find(tmp_event_id) == events.end())
      events[tmp_event_id] = Event(i);
    else
      events[tmp_event_id].loc.push_back(i);

    Event * event_ptr = &events[tmp_event_id];

    while ((++i < N) && (tmp_event_id == id_events[i]))
      event_ptr->loc.push_back(i);

  }

  // Now, iterating throughout the events
  for (auto & e : events)
  {

    Rprintf("Event id: %i\nIndividuals:", e.first);
    for (auto & i : e.second.loc)
      Rprintf("%i ", i);
    Rprintf("/eol\n");

  }

  return ans;

}

/***R
# Case 1
exposure(
  sort(rep(1:5, 2)),
  sort(rep(c(1,2), 5)),
  sort(rep(c(1,2), 5)),
  rep(c(0,1), 5)
)

set.seed(1231)
exposure(
  id_indiv  = c(1,2,3, 2,3,4, 1,3),
  id_events = c(1,1,1, 2,2,2, 3,3),
  time      = c(1,1,1, 2,2,2, 3,3),
  actions   = sample.int(2, 8, replace = TRUE)
)

*/
