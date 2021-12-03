#include <Rcpp.h>
#include <map>
using namespace Rcpp;

struct Event;

struct Person {
  int id;
  bool pointed_past;

  // Running counts
  std::vector< int > cumsum;
  std::vector< Event*> events;

  Person(int id_) : id(id_), pointed_past(false), cumsum(0u), events(0u) {};

};

struct Event {
  std::vector< Person* > Persons;
  int time;
  Event(int time_) : time(time_) {};
};

// Data must be sorted by:
// - time
// - event
// - id
// [[Rcpp::export(name = "compute_exposure.")]]
IntegerVector compute_exposure(
  const IntegerVector & time,
  const IntegerVector & events,
  const IntegerVector & ids,
  const IntegerVector & acted
) {

  // Initializing the map for individuals
  std::map<int, Event> Events;
  std::map<int, Person> Persons;
  IntegerVector ans(time.size());

  // Initializing events and persons
  Events[events[0u]] = Event(time[0u]);
  Persons[ids[0u]]   = Person(ids[0u]);

  // Pointing to the first event and adding the first person
  Event * event_ptr = &Events[events[0u]];
  event_ptr->Persons.push_back(&Persons[ids[0u]]);
  Persons[ids[0u]].cumsum.push_back(0);
  Persons[ids[0u]].events.push_back(event_ptr);

  int N = time.size();
  for (int i = 1; i < N; ++i)
  {

    // Checking individual
    int id = ids[i];
    int ev = events[i];

    // This is a new event, and we need to initialize it
    if (ev != events[i - 1])
    {
      // Creating new event, and pointing to it
      Events[ev] = Event(time[i]);
      event_ptr  = &Events[ev];

    }

    // Adding individual
    Person * person_ptr;
    auto tmp_person = Persons.find(id);
    bool person_exists = (tmp_person != Persons.end());
    if (!person_exists)
      Persons[id] = Person(id);

    // Making pointer, adding it to the event and vice-versa
    person_ptr = &Persons[id];
    person_ptr->cumsum.push_back(acted[id]);
    person_ptr->events.push_back(event_ptr);
    event_ptr->Persons.push_back(person_ptr);

    // Updating cumsum
    if (person_exists)
    {
      person_ptr->cumsum[person_ptr->cumsum.size() - 1] +=
        person_ptr->cumsum[person_ptr->cumsum.size() - 2];
    }


  }


}
