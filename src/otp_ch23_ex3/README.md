# Load Balancer with Servers

The Load Balancer (LB) should send Requests to Worker with least Load

### Load Balancer

**todo**

- can keep a state of how many requests each Server is handling
- when a Server is sent a request, the Load is *incremented* on the LB
- when a Server completes a request, it tells the LB, and the Load is *decremented*
- If there is a Load tie, a Server should be picked at random
- If a Server dies, it's Load counter should be purged from the LB State
- The EventHandler should log all State changes

**done**

- When a new Server joins, it's Load should be initialized as 0 on the LB's State
