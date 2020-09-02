#include <Rcpp.h>
#include <random>
using namespace Rcpp;


template <typename T>
std::vector<size_t> sort_indexes(const std::vector<T> &v) {
  
  // initialize original index locations
  std::vector<size_t> idx(v.size());
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
  std::mt19937 engine;
  
  Parameters(double female_, double years_, double rho_, double exposure_) :
    female(female_), years(years_), rho(rho_), exposure(exposure_) {};
  
  void seed(int s) {engine.seed(s);};
};

/**A police officer has
 * @param female,years covariates
 * @param rate a speed at which they will react (initiative). Reaction time is
 * a poisson process.
 */
class officer {

public:
  bool female;
  int years;
  double rate;
  int id;
  std::vector< bool > previous_mult;
  std::vector< bool > previous_expo;
  
  std::exponential_distribution<> react_time;
  
  officer(bool female_, int years_, double rate_, int id_) :
    female(female_), years(years_), rate(rate_), id(id_),
    previous_mult(0u), previous_expo(0u) {
    
    // Generates its own reaction time
    react_time = std::exponential_distribution<>(rate);
    
  };
  ~officer() {};
  
};

/**The actual event. Has multiple officers. Their reactions are a function
 * of the model parameters.
 */
class Event {
public:
  int id;
  std::vector< officer* > officers;
  std::vector< bool > pointed;
  Event(int id_) : id(id_), officers(0u), pointed(0u) {};
  ~Event() {};
  
  void add_officer(officer & o) {
    officers.push_back(&o);
    pointed.push_back(false);
    return;
  }
  
  unsigned int size() const {return officers.size();};
  
  void point(Parameters & p);
};



inline void Event::point(Parameters & p) {
  
  // An indicator vector telling whether the officer reacted  
  std::vector< double > times(officers.size(), 0.0);
  std::vector< int > order(times.size(), 0);
  
  // Uniform between 0 and 1
  std::uniform_real_distribution<> rand_unif;

  // Drawing the first number
  for (auto i = 0u; i < officers.size(); ++i)
    times[i] = officers[i]->react_time(p.engine);
  
  // Getting who goes first
  std::vector< size_t > ord = sort_indexes(times);
  
  // Now, iterating thought the index
  int n_pointed = 0;
  for (auto i = 0u; i < ord.size(); ++i) {
    
    // Baseline parameters
    double val =
      officers[i]->female * p.female +
      officers[i]->years  * p.years;
    
    // Prev Exposure effect?
    // std::cout << "Printing " << officers[i]->previous_expo.size() << std::endl;
    if (officers[i]->previous_mult.size() > 0) {
      if (officers[i]->previous_expo[officers[i]->previous_expo.size()])
        val += p.exposure;

    }
    
    // Has anyone pointed so far?
    if (n_pointed > 0)
      val += p.rho;
    
    // Will the officer point?
    if (rand_unif(p.engine) < (exp(val)/(1 + exp(val)))) {
      pointed[i] = true;
      n_pointed++;
    }
    
  }
  
  // Updating the officer's history
  bool multiple = officers.size() > 1u;
  for (auto i = 0u; i < officers.size(); ++i) {
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

  std::vector< Event > dat;
  
  Events() {};
  ~Events() {};
  
  void add_event(Event e, Parameters & params) {
    dat.push_back(e);
    
    // Running the experiment
    dat[dat.size() - 1].point(params);
    
    return;
  }
  
  Event operator[](int i) const {
    return dat[i];
  }
  
};

class Simulation {
public:

  int nevents;
  int nofficers;
  int min_per_event;
  int max_per_event;
  int min_year;
  int max_year;
  int min_rate;
  int max_rate;
  Parameters params;
  
  Events events;
  std::vector< officer > officers;
  
  Simulation(
    int nevents_,
    int nofficers_,
    int min_per_event_,
    int max_per_event_,
    int min_year_,
    int max_year_,
    int min_rate_,
    int max_rate_,
    double female_,
    double years_,
    double rho_,
    double exposure_,
    int seed_
  ) : nevents(nevents_), nofficers(nofficers_), min_per_event(min_per_event_),
  max_per_event(max_per_event_), min_year(min_year_), max_year(max_year_),
  min_rate(min_rate_), max_rate(max_rate_),
  params(female_, years_, rho_, exposure_) {
    
    // Setting up the seed and random numbers;
    params.seed(seed_);
    
    std::uniform_real_distribution<> unif;
    std::uniform_int_distribution<> ryears(min_year, max_year);
    std::uniform_real_distribution<> rrates(min_rate, max_rate);
    
    // Generating the officers
    for (auto i = 0u; i < nofficers; ++i) {
      
      officers.push_back(
        officer(
          unif(params.engine) > .5,
          ryears(params.engine),
          rrates(params.engine),
          i
      )
      );
      
    }
    
    std::uniform_int_distribution<> noff(min_per_event, max_per_event);
    std::uniform_int_distribution<> rid(0, nofficers - 1);
    
    // Generating the events
    for (auto i = 0u; i < nevents; ++i) {
      
      // Empty event
      Event e(i);
      
      // How many first
      int event_size = std::min(noff(params.engine), nofficers);
      
      // Sampling
      int ntries = 0;
      while (event_size > 0 & ++ntries < 1000) {
        int proposed_officer = rid(params.engine);
        for (auto j = 0; j < e.size(); ++j) {
          if (proposed_officer == e.officers[j]->id)
            continue;
        }
        e.add_officer(officers[proposed_officer]);
        event_size--;
          
      }
      
      // Adding the event and computing what happens
      events.add_event(e, params);
    }
    
  }
  
  std::vector< std::vector< double > > get_data();
  
};

std::vector< std::vector< double > > Simulation::get_data() {
  
  // Writing the reports
  std::vector< std::vector<double> > ans;
  for (auto i = 0u; i < nevents; ++i) {
    for (auto j = 0u; j < events[i].size(); ++j) {
      ans.push_back({
        (double) events[i].officers[j]->id,
        (double) events[i].officers[j]->female,
        (double) events[i].officers[j]->years,
        (double) events[i].id,
        (double) events[i].pointed[j]
      });
    }
  }
  
  return ans;

}


//' Simulate Police Force Events
//' 
//' This function generates data similar to that featured in the paper. Events
//' are drawn at random, as the number of officers per event. The outcome variable,
//' whether the officer points his gun or not, is drawn sequentially as a poisson
//' process.
//' 
//' @param nevents,nofficers Integers. Number of events and officers to simulate.
//' @param min_per_event,max_per_event Integers. Lower and upper bounds for the 
//' number of officers in the event.
//' @param min_year,max_years Integers. Lower and upper bounds for the number
//' of years of experience of the officers.
//' @param min_rate,max_rate Doubles. Lower and upper bounds for the reaction
//' rates (see details).
//' @param female,years,rho,exposure Doubles. Parameters (coefficients) for
//' the logistic probabilities.
//' @param seed Integer. Seed for the pseudo-number generation.
//' 
//' @details
//' The simulation process goes as follow:
//' 1. The officers are simulated. Female ~ Bernoulli(0.5),
//'    Action rate ~ Unif(min_rate, max_rate),
//'    Years of experience ~ Discrete Unif[min_years, max_year]
//' 2. Events are simulated, each event has a nofficers ~ Discrete Unif[min_per_event, max_per_event]
//'    Once the event is done, a sequence of reaction is given by each officers'
//'    action rate (Poisson process). Whether an officer points or not is set by
//'    a logistic model
//'    
//'    point ~ female + years of experience + has any pointed? + previous exposure
//'    
//'    The corresponding parameters are as specified by the user. Events are simulated
//'    one at a time.
//' @returns
//' 
//' @export
// [[Rcpp::export]]
std::vector< std::vector< double > > simulate_njforce(
  int nevents,
  int nofficers,
  int min_per_event = 1,
  int max_per_event = 5,
  int min_year = 0,
  int max_year = 10,
  int min_rate = 5,
  int max_rate = 5,
  double female = -.5,
  double years = -.5,
  double rho = 0,
  double exposure = .5,
  int seed = 123
) {
  
  Simulation sim(
      nevents, nofficers, min_per_event, max_per_event,
      min_year, max_year, min_rate, max_rate, female, years, rho,
      exposure, seed
  );
  
  return sim.get_data();
  
}

/***R
ans <- simulate_njforce(10000, 1000, rho_ = 0, exposure_ = 0)
ans <- do.call(rbind, ans)
colnames(ans) <- c("officerid", "female", "years", "incidentid", "pointed")
ans <- as.data.frame(ans)

summary(glm(pointed ~ -1 + female + years, data = ans, family = binomial("logit")))
*/
