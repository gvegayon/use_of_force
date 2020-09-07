#include <Rcpp.h>
#include <random>
using namespace Rcpp;

template <typename T>
using Vec = std::vector< T >;

template <typename T>
Vec<size_t> sort_indexes(const Vec<T> &v) {

  // initialize original index locations
  Vec<size_t> idx(v.size());
  std::iota(idx.begin(), idx.end(), 0);

  // sort indexes based on comparing values in v
  // using std::stable_sort instead of std::sort
  // to avoid unnecessary index re-orderings
  // when v contains elements of equal values
  stable_sort(idx.begin(), idx.end(),
              [&v](size_t i1, size_t i2) {return v[i1] < v[i2];});

  return idx;
}

class Parameters {
public:

  double female;
  double years;
  double rho;
  double exposure;
  double context;
  double fixed_effect;
  std::mt19937 engine;
  std::uniform_real_distribution<> rand_unif;
  std::normal_distribution<> rand_normal;

  Parameters(
    double female_, double years_, double rho_, double exposure_,
    double context_, double fixed_effect_) :
    female(female_), years(years_), rho(rho_), exposure(exposure_),
    context(context_), fixed_effect(fixed_effect_) {};

  void seed(int s) {engine.seed(s);};

  void set_par(
      double female_, double years_, double rho_, double exposure_,
      double context_, double fixed_effect_) {
    female       = female_;
    years        = years_;
    rho          = rho_;
    exposure     = exposure_;
    context      = context_;
    fixed_effect = fixed_effect_;
    return;
  }
};

/**A police officer has
 * @param female,years covariates
 * @param rate a speed at which they will react (initiative). Reaction time is
 * a poisson process.
 */
class Officer {

public:
  bool female;
  int years;
  double rate;
  double fixed_effect;
  int id;
  Vec< bool > previous_mult;
  Vec< bool > previous_expo;

  std::exponential_distribution<> react_time;

  Officer() : female(true), years(0), rate(1.0), fixed_effect(0), id(-1),
    previous_mult(0u), previous_expo(0u) {

    react_time = std::exponential_distribution<>(rate);

  };

  Officer(bool female_, int years_, double rate_, double fixed_effect_, int id_) :
    female(female_), years(years_), rate(rate_), fixed_effect(fixed_effect_),
    id(id_), previous_mult(0u), previous_expo(0u) {

    // Generates its own reaction time
    react_time = std::exponential_distribution<>(rate);

  };
  ~Officer() {};

  void reset();

};

inline void Officer::reset() {
  previous_mult.clear();
  previous_expo.clear();
  return;
}

class Officers {
public:
  Vec< Officer* > dat;
  Vec< unsigned int > to_delete;
  Officers(): dat(0u) {};

  ~Officers() {
    for (auto iter = to_delete.begin(); iter != to_delete.end(); ++iter)
      delete dat[*iter];
    return;
  }

  void add(
      bool female_, int years_, double rate_, double fixed_effect_, int id_
    ) {
    dat.push_back(new Officer(female_, years_, rate_, fixed_effect_, id_));
    to_delete.push_back(dat.size() - 1u);
    return;
  }

  Officer * operator[](unsigned int i) {
    return dat[i];
  }

  unsigned int size() const {return dat.size();};
};

/**The actual event. Has multiple officers. Their reactions are a function
 * of the model parameters.
 */
class Event {
public:
  int id;
  Vec< Officer* > officers;
  Vec< bool > pointed;
  Vec< double > years;
  Vec< double > response_time;
  Vec< bool > not_first;
  Vec< bool > exposed;
  double violence_level;

  Event() : id(-1), officers(0u), pointed(0u), years(0u), response_time(0u),
    not_first(0u), exposed(0u) {};
  Event(int id_) : id(id_), officers(0u), pointed(0u), years(0u),
  response_time(0u), not_first(0u), exposed(0u) {};

  ~Event() {
    for (unsigned int i = 0; i < officers.size(); ++i)
      officers[i] = nullptr;
  };

  void add_officer(Officer * o, int years_ = -1) {
    officers.push_back(o);
    if (years_ < 0)
      years.push_back(o->years);
    else
      years.push_back(years_);

    response_time.push_back(0);
    pointed.push_back(false);
    not_first.push_back(true);
    exposed.push_back(false);
    return;
  }

  unsigned int size() const {return officers.size();};

  void point(Parameters & p);
};

inline void Event::point(Parameters & p) {

  // An indicator vector telling whether the officer reacted
  Vec< int > order(this->size(), 0);

  // Figuring out the level of violence
  violence_level = p.rand_normal(p.engine);

  // Drawing the first number
  for (unsigned int i = 0u; i < officers.size(); ++i)
    response_time[i] = officers[i]->react_time(p.engine);

  // Getting who goes first
  Vec< size_t > ord = sort_indexes(response_time);

  // Now, iterating thought the index
  int n_pointed = 0;
  for (unsigned int i = 0u; i < ord.size(); ++i) {

    if (i == 0u)
      not_first[ord[i]] = false;
    else
      not_first[ord[i]] = true;

    // Baseline parameters
    double val =
      officers[ord[i]]->female * p.female +
      years[ord[i]]            * p.years +
      violence_level      * p.context +
      officers[ord[i]]->fixed_effect * p.fixed_effect;

    // Prev Exposure effect?
    exposed[ord[i]] = false;
    if (officers[ord[i]]->previous_expo.size() > 0) {
      if (officers[ord[i]]->previous_expo[officers[ord[i]]->previous_expo.size() - 1]) {
        val += p.exposure;
        exposed[ord[i]] = true;
      }

    }

    // Has anyone pointed so far?
    if (n_pointed > 0)
      val += p.rho;

    // Will the officer point?
    if (p.rand_unif(p.engine) < (exp(val)/(1 + exp(val)))) {
      pointed[ord[i]] = true;
      n_pointed++;
    } else
      pointed[ord[i]] = false;

  }

  // Updating the officer's history
  bool multiple = officers.size() > 1u;
  for (unsigned int i = 0u; i < officers.size(); ++i) {
    officers[i]->previous_mult.push_back(multiple);

    // Only exposed if I was not the one who pointed
    if (multiple) {
      if (!pointed[i] & (n_pointed > 0))
        officers[i]->previous_expo.push_back(true);
      else if (pointed[i] & (n_pointed > 1))
        officers[i]->previous_expo.push_back(true);
      else
        officers[i]->previous_expo.push_back(false);
    } else
      officers[i]->previous_expo.push_back(false);

  }

  return ;
}

class Events {
public:

  Vec< Event * > dat;
  Vec< unsigned int > to_delete;

  Events() {};
  ~Events() {
    for (auto iter = to_delete.begin(); iter != to_delete.end(); ++iter)
      delete dat[*iter];
  };

  void add_event(Event & e) {
    dat.push_back(&e);
    return;
  }

  void add_event(int i) {
    dat.emplace_back(new Event(i));
    to_delete.push_back(dat.size() - 1);
    return;
  }


  void run(Parameters & params) {
    for (unsigned int i = 0u; i < dat.size(); ++i) {
      dat[i]->point(params);
    }
  }

  Event * & operator[](int i) {
    return dat[i];
  }

  unsigned int size() const {return dat.size();}

};

class Simulation {
public:

  unsigned int nevents;
  unsigned int nofficers;
  unsigned int min_per_event;
  unsigned int max_per_event;
  unsigned int min_year;
  unsigned int max_year;
  unsigned int min_rate;
  unsigned int max_rate;
  Parameters params;

  Events events;
  Officers officers;

  Simulation(
    Vec< int > eventid,
    Vec< int > officerid,
    Vec< bool > female,
    Vec< double > rate,
    Vec< double > fixed_effect,
    Vec< int > years,
    double female_,
    double years_,
    double rho_,
    double exposure_,
    double context_,
    double fixed_effect_,
    int seed_
  );

  Simulation(
    unsigned int nevents_,
    unsigned int nofficers_,
    unsigned int min_per_event_,
    unsigned int max_per_event_,
    unsigned int min_year_,
    unsigned int max_year_,
    unsigned int min_rate_,
    unsigned int max_rate_,
    double female_,
    double years_,
    double rho_,
    double exposure_,
    double context_,
    double fixed_effect_,
    int seed_
  );

  Vec< Vec< double > > get_data(bool byrow = true);
  Vec< double > get_response();
  void run(bool reset = false) {

    if (reset)
      for (unsigned int i = 0u; i < officers.size(); ++i)
        officers[i]->reset();

    events.run(params);

    return;
  }

  void run(
    double female,
    double years,
    double rho,
    double exposure,
    double context,
    double fixed_effect
  ) {
    params.set_par(female, years, rho, exposure, context, fixed_effect);
    this->run(true);
    return;
  }

  // void get_y

};

inline Simulation::Simulation(
    Vec< int > eventid,
    Vec< int > officerid,
    Vec< bool > female,
    Vec< double > rate,
    Vec< double > fixed_effect,
    Vec< int > years,
    double female_,
    double years_,
    double rho_,
    double exposure_,
    double context_,
    double fixed_effect_,
    int seed_
) : params(female_, years_, rho_, exposure_, context_, fixed_effect_) {
  params.seed(seed_);

  // Creating a map
  std::unordered_map<int, int> officer_map;
  std::unordered_map<int, int> event_map;

  for (unsigned int i = 0u; i < eventid.size(); ++i) {

    // Does the officer exists
    auto officer_loc = officer_map.find(officerid[i]);
    int officer_idx = 0;
    if (officer_loc == officer_map.end()) {

      officer_map[officerid[i]] = officers.size();
      officers.add(female[i], years[i], rate[i], fixed_effect[i], officerid[i]);
      officer_idx = officers.size() - 1;

    } else
      officer_idx = officer_loc->second;

    // Does the event exists?
    auto event_loc = event_map.find(eventid[i]);
    int event_idx = 0;
    if (event_loc == event_map.end()) {

      event_map[eventid[i]] = events.size();
      events.add_event(eventid[i]);
      event_idx = events.size() - 1;

    } else
      event_idx = event_loc->second;

    // Adding the officer to the event
    events[event_idx]->add_officer(
        officers[officer_idx],
                years[i]
    );

  }

  nevents = events.size();
  nofficers = officers.size();

  return;

}

inline Simulation::Simulation(
    unsigned int nevents_,
    unsigned int nofficers_,
    unsigned int min_per_event_,
    unsigned int max_per_event_,
    unsigned int min_year_,
    unsigned int max_year_,
    unsigned int min_rate_,
    unsigned int max_rate_,
    double female_,
    double years_,
    double rho_,
    double exposure_,
    double context_,
    double fixed_effect_,
    int seed_
) : nevents(nevents_), nofficers(nofficers_), min_per_event(min_per_event_),
max_per_event(max_per_event_), min_year(min_year_), max_year(max_year_),
min_rate(min_rate_), max_rate(max_rate_),
params(female_, years_, rho_, exposure_, context_, fixed_effect_) {

  // Setting up the seed and random numbers;
  params.seed(seed_);

  std::uniform_int_distribution<> ryears(min_year, max_year);
  std::uniform_real_distribution<> rrates(min_rate, max_rate);

  // Generating the officers
  for (unsigned int i = 0u; i < nofficers; ++i) {

    officers.add(
      params.rand_unif(params.engine) > .5,
      ryears(params.engine),
      rrates(params.engine),
      params.rand_normal(params.engine),
      i
    );

  }

  std::uniform_int_distribution<> noff(min_per_event, max_per_event);
  std::uniform_int_distribution<> rid(0, nofficers - 1);

  // Generating the events
  for (unsigned int i = 0u; i < nevents; ++i) {

    // Empty event
    events.add_event(i);

    // How many first
    int event_size = std::min(noff(params.engine), (int) nofficers);

    // Sampling
    int ntries = 0;
    while ((event_size > 0) & (++ntries < 1000)) {
      int proposed_officer = rid(params.engine);
      for (unsigned int j = 0u; j < events[i]->size(); ++j) {
        if (proposed_officer == events[i]->officers[j]->id)
          continue;
      }
      events[i]->add_officer(officers[proposed_officer]);
      event_size--;

    }

  }

  return;

}

Vec< Vec< double > > Simulation::get_data(bool byrow) {

  // Writing the reports
  Vec< Vec<double> > ans;

  if (byrow) {
    for (unsigned int i = 0u; i < nevents; ++i) {
      for (unsigned int j = 0u; j < events[i]->size(); ++j) {
        ans.push_back({
          (double) events[i]->officers[j]->id,
          (double) events[i]->officers[j]->female,
          (double) events[i]->years[j],
          (double) events[i]->officers[j]->fixed_effect,
          (double) events[i]->id,
          (double) events[i]->violence_level,
          (double) events[i]->not_first[j],
          (double) events[i]->exposed[j],
          (double) events[i]->pointed[j]
        });
      }
    }
  } else {

    ans.resize(10u);

    for (unsigned int i = 0u; i < nevents; ++i) {
      for (unsigned int j = 0u; j < events[i]->size(); ++j) {

        ans[0u].push_back((double) events[i]->officers[j]->id);
        ans[1u].push_back((double) events[i]->officers[j]->female);
        ans[2u].push_back((double) events[i]->years[j]);
        ans[3u].push_back((double) events[i]->officers[j]->fixed_effect);
        ans[4u].push_back((double) events[i]->id);
        ans[5u].push_back((double) events[i]->violence_level);
        ans[6u].push_back((double) events[i]->response_time[j]);
        ans[7u].push_back((double) !events[i]->not_first[j]);
        ans[8u].push_back((double) events[i]->exposed[j]);
        ans[9u].push_back((double) events[i]->pointed[j]);

      }
    }

  }

  return ans;

}

inline Vec< double > Simulation::get_response() {

  Vec< double > res;
  for (unsigned int i = 0u; i < events.size(); ++i)
    for (unsigned int j = 0u; j < events[i]->size(); ++j)
      res.push_back((double) events[i]->pointed[j]);

  return res;

}



// Simulate Police Force Events
//
// This function generates data similar to that featured in the paper. Events
// are drawn at random, as the number of officers per event. The outcome variable,
// whether the officer points his gun or not, is drawn sequentially as a poisson
// process.
//
// @param nevents,nofficers Integers. Number of events and officers to simulate.
// @param min_per_event,max_per_event Integers. Lower and upper bounds for the
// number of officers in the event.
// @param min_year,max_years Integers. Lower and upper bounds for the number
// of years of experience of the officers.
// @param min_rate,max_rate Doubles. Lower and upper bounds for the reaction
// rates (see details).
// @param female_par,years_par,rho_par,exposure_par Doubles. Parameters (coefficients) for
// the logistic probabilities.
// @param seed Integer. Seed for the pseudo-number generation.
//
// @details
// The simulation process goes as follow:
// 1. The officers are simulated. Female ~ Bernoulli(0.5),
//    Action rate ~ Unif(min_rate, max_rate),
//    Years of experience ~ Discrete Unif[min_years, max_year]
// 2. Events are simulated, each event has a nofficers ~ Discrete Unif[min_per_event, max_per_event]
//    Once the event is done, a sequence of reaction is given by each officers'
//    action rate (Poisson process). Whether an officer points or not is set by
//    a logistic model
//
//    point ~ female + years of experience + has any pointed? + previous exposure
//
//    The corresponding parameters are as specified by the user. Events are simulated
//    one at a time.
// @returns
// A data frame with the following columns
// - Officer id
// - Whether the officer is female
// - Years of experience
// - Incident id
// - Whether the officer pointed a gun
//
// Each row represents one report per officer involved in the event.
// @export
// @examples
// x <- simulate_njforce(1000, 400)
// [[Rcpp::export(rng = false, name = "sim_events_cpp")]]
std::vector< std::vector< double > > sim_events(
  int nevents,
  int nofficers,
  int min_per_event = 1,
  int max_per_event = 5,
  int min_year = 0,
  int max_year = 10,
  int min_rate = 5,
  int max_rate = 5,
  double female_par = -.5,
  double years_par = -.5,
  double rho_par = 0,
  double exposure_par = .5,
  double context_par = 1.0,
  double fixed_effect_par = 1.0,
  int nsims = 1,
  int seed = 123
) {

  // Preparing the simulation object
  Simulation sim(
      nevents, nofficers, min_per_event, max_per_event,
      min_year, max_year, min_rate, max_rate, female_par, years_par, rho_par,
      exposure_par, context_par, fixed_effect_par, seed
  );

  // Running the simulation
  sim.run();

  Vec< Vec< double > > ans(sim.get_data(false));

  if (nsims > 1) {
    // Re-running and recovering only the behavior
    for (int i = 1u; i < nsims; ++i) {
      sim.run(true);
      ans.push_back(sim.get_response());
    }
  }

  return ans;

}

// [[Rcpp::export(rng = false, name = "sim_events2_cpp")]]
std::vector<std::vector<double>> sim_events2(
  std::vector< int > incidentid,
  std::vector< int > officerid,
  std::vector< bool > female,
  std::vector< double > rate,
  std::vector< double > fixed_effect,
  std::vector< int > years,
  double female_par,
  double years_par,
  double rho_par,
  double exposure_par,
  double context_par,
  double fixed_effect_par,
  int nsims,
  int seed
) {

  // Creating the simulation object
  Simulation sim(
      incidentid, officerid, female, rate, fixed_effect, years, female_par,
      years_par, rho_par, exposure_par, context_par, fixed_effect_par, seed
    );

  // Running the simulation
  sim.run();

  Vec< Vec< double > > ans(sim.get_data(false));

  if (nsims > 1) {
    // Re-running and recovering only the behavior
    for (int i = 1u; i < nsims; ++i) {
      sim.run(true);
      ans.push_back(sim.get_response());
    }
  }

  return ans;

}

/***R
ans <- simulate_njforce(10000, 1000, rho = 1, exposure = 1)

summary(glm(pointed ~ -1 + female + years, data = ans, family = binomial("logit")))
*/
