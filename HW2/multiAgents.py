# multiAgents.py
# --------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


from util import manhattanDistance
from game import Directions
import random, util

from game import Agent


class ReflexAgent(Agent):
    """
      A reflex agent chooses an action at each choice point by examining
      its alternatives via a state evaluation function.

      The code below is provided as a guide.  You are welcome to change
      it in any way you see fit, so long as you don't touch our method
      headers.
    """

    def getAction(self, gameState):
        """
        You do not need to change this method, but you're welcome to.

        getAction chooses among the best options according to the evaluation function.

        Just like in the previous project, getAction takes a GameState and returns
        some Directions.X for some X in the set {North, South, West, East, Stop}
        """
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions()

        # Choose one of the best actions
        scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices)  # Pick randomly among the best

        "Add more of your code here if you want to"

        return legalMoves[chosenIndex]

    def evaluationFunction(self, currentGameState, action):
        """
        Design a better evaluation function here.

        The evaluation function takes in the current and proposed successor
        GameStates (pacman.py) and returns a number, where higher numbers are better.

        The code below extracts some useful information from the state, like the
        remaining food (newFood) and Pacman position after moving (newPos).
        newScaredTimes holds the number of moves that each ghost will remain
        scared because of Pacman having eaten a power pellet.

        Print out these variables to see what you're getting, then combine them
        to create a masterful evaluation function.
        """
        # Useful information you can extract from a GameState (pacman.py)
        successorGameState = currentGameState.generatePacmanSuccessor(action)
        newPos = successorGameState.getPacmanPosition()
        newFood = successorGameState.getFood()
        newGhostStates = successorGameState.getGhostStates()
        newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]

        "*** YOUR CODE HERE ***"
        # if pacman trys to stay in the same place, stop it
        if currentGameState.getPacmanPosition() == successorGameState.getPacmanPosition():
            return -999999

        # get a the food and ghost positions
        newFood = newFood.asList()
        gridGhostStates = currentGameState.getGhostPositions()

        # here we look at the food on the board
        # generally the closer food is the more pacman should go towards it
        # maybe and inverse function for how little food there is to make movement better when little food remains
        foodScore = 0
        for i in newFood:
            # foodScore += 4/(util.manhattanDistance(newPos, i)+1)
            dist = util.manhattanDistance(newPos, i)
            if dist <= 3:
                foodScore += 1.5
            elif dist <= 6:
                foodScore += .75
            else:
                foodScore += .4
        # here we look we look at where the ghosts are
        # closer ghosts of course are bad and far away ones are ok but not great
        closestScore = 0
        for i in gridGhostStates:
            # closestScore += 6/(util.manhattanDistance(newPos, i)+1)
            if i == newPos:  # stop pacman from going into a ghost
                return -999999999
            dist = util.manhattanDistance(newPos, i)
            if dist <= 2:
                closestScore += 64
            elif dist <= 5:
                closestScore += 16
            else:
                closestScore += 8

        # print "food:",foodScore
        # print "close:",closestScore

        # final score equation; must use the state score otherwise pacman has issues
        score = successorGameState.getScore() + foodScore - closestScore
        # print successorGameState.getScore()
        # print score
        return score


def scoreEvaluationFunction(currentGameState):
    """
      This default evaluation function just returns the score of the state.
      The score is the same one displayed in the Pacman GUI.

      This evaluation function is meant for use with adversarial search agents
      (not reflex agents).
    """
    return currentGameState.getScore()


class MultiAgentSearchAgent(Agent):
    """
      This class provides some common elements to all of your
      multi-agent searchers.  Any methods defined here will be available
      to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

      You *do not* need to make any changes here, but you can if you want to
      add functionality to all your adversarial search agents.  Please do not
      remove anything, however.

      Note: this is an abstract class: one that should not be instantiated.  It's
      only partially specified, and designed to be extended.  Agent (game.py)
      is another abstract class.
    """

    def __init__(self, evalFn='scoreEvaluationFunction', depth='2'):
        self.index = 0  # Pacman is always agent index 0
        self.evaluationFunction = util.lookup(evalFn, globals())
        self.depth = int(depth)


class MinimaxAgent(MultiAgentSearchAgent):
    """
      Your minimax agent (question 2)
    """

    def getAction(self, gameState):
        """
          Returns the minimax action from the current gameState using self.depth
          and self.evaluationFunction.

          Here are some method calls that might be useful when implementing minimax.

          gameState.getLegalActions(agentIndex):
            Returns a list of legal actions for an agent
            agentIndex=0 means Pacman, ghosts are >= 1

          gameState.generateSuccessor(agentIndex, action):
            Returns the successor game state after an agent takes an action

          gameState.getNumAgents():
            Returns the total number of agents in the game
        """
        "*** YOUR CODE HERE ***"

        def minmax(depth, game, agentIndex=0):
            # reminder, game is the currentgame state-> it changes at each recursion
            # depth goes to 0

            # here we determine if we have ran all the ghost moves
            # if so, reduce the depth and reset the agent
            if agentIndex == game.getNumAgents():
                depth -= 1
                agentIndex = 0

            moves = game.getLegalActions(agentIndex)

            # here is where we check for terminal state or max depth
            if depth == 0 or not moves:
                return self.evaluationFunction(game), 0

            v1 = None  # we must set an initial score value; None must be used by default for how this minimax is set up
            m1 = 0

            # if we are running pacman
            if agentIndex == 0:
                for i in moves:
                    v2, m2 = minmax(depth, game.generateSuccessor(agentIndex, i), agentIndex + 1)
                    if v1 is None or v2 > v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                return v1, m1
            # if we are running ghosts
            else:
                for i in moves:
                    v2, m2 = minmax(depth, game.generateSuccessor(agentIndex, i), agentIndex + 1)
                    if v1 is None or v2 < v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                return v1, m1

        v, m = minmax(self.depth, gameState)
        return m
        # util.raiseNotDefined()


class AlphaBetaAgent(MultiAgentSearchAgent):
    """
      Your minimax agent with alpha-beta pruning (question 3)
    """

    def getAction(self, gameState):
        """
          Returns the minimax action using self.depth and self.evaluationFunction
        """
        "*** YOUR CODE HERE ***"

        def alphabeta(depth, game, alpha=float('-inf'), beta=float('inf'), agentIndex=0):
            # reminder, game is the currentgame state-> it changes at each recursion
            # depth goes to 0

            # here we determine if we have ran all the ghost moves
            # if so, reduce the depth and reset the agent
            if agentIndex == game.getNumAgents():
                depth -= 1
                agentIndex = 0

            moves = game.getLegalActions(agentIndex)

            v1 = None  # we must set an initial score value; None must be used by default for how this minimax is set up
            m1 = 0
            a1 = alpha
            b1 = beta

            # here is where we check for terminal state or max depth
            if depth == 0 or not moves:
                return self.evaluationFunction(game), 0, a1, b1

            # if we are running pacman
            if agentIndex == 0:
                for i in moves:
                    v2, m2, a2, b2 = alphabeta(depth, game.generateSuccessor(agentIndex, i), a1, b1, agentIndex + 1)
                    if v1 is None or v2 > v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                    a1 = max(a1, v1)
                    if b1 < a1:  # beta cut off; we know there are no better moves to be found here
                        return v1, m1, a1, b1
                return v1, m1, a1, b1
            # if we are running ghosts
            else:
                for i in moves:
                    v2, m2, a2, b2 = alphabeta(depth, game.generateSuccessor(agentIndex, i), a1, b1, agentIndex + 1)
                    if v1 is None or v2 < v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                    b1 = min(b1, v1)
                    if b1 < a1:  # beta cut; we know there are no better moves to be found here
                        return v1, m1, a1, b1
                return v1, m1, a1, b1

        v, m, a, b = alphabeta(self.depth, gameState)
        return m


class ExpectimaxAgent(MultiAgentSearchAgent):
    """
      Your expectimax agent (question 4)
    """

    def getAction(self, gameState):
        """
          Returns the expectimax action using self.depth and self.evaluationFunction

          All ghosts should be modeled as choosing uniformly at random from their
          legal moves.
        """
        "*** YOUR CODE HERE ***"

        def expectimax(depth, game, agentIndex=0):
            # reminder, game is the currentgame state-> it changes at each recursion
            # depth goes to 0

            # here we determine if we have ran all the ghost moves
            # if so, reduce the depth and reset the agent
            if agentIndex == game.getNumAgents():
                depth -= 1
                agentIndex = 0

            moves = game.getLegalActions(agentIndex)

            # here is where we check for terminal state or max depth
            if depth == 0 or not moves:
                return self.evaluationFunction(game), 0

            v1 = None  # we must set an initial score value; None must be used by default for how this minimax is set up
            m1 = 0
            s1 = []

            # if we are running pacman
            if agentIndex == 0:
                for i in moves:
                    v2, m2 = expectimax(depth, game.generateSuccessor(agentIndex, i), agentIndex + 1)
                    if v1 is None or v2 > v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                return v1, m1
            # if we are running ghosts
            else:
                for i in moves:
                    v2, m2 = expectimax(depth, game.generateSuccessor(agentIndex, i), agentIndex + 1)
                    s1.append(v2)
                    if v1 is None or v2 < v1:
                        # if we found a better move or the value is default; update
                        v1 = v2
                        m1 = i
                # Expectimax is pretty much the exact same as minmax but now we the moves have weight
                # this case I just made the weights the average of all moves possible
                return float(sum(s1)) / float(len(s1)), m1

        v, m = expectimax(self.depth, gameState)
        return m


def betterEvaluationFunction(currentGameState):
    """
      Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
      evaluation function (question 5).

      DESCRIPTION: Basic logic, food is great, ghosts are bad; pellets are good but also hurt us because that is time
       not getting food however eating a ghost is worth a lot so we must consider that. multipliers are tuned for states
        not actions.
    """
    "*** YOUR CODE HERE ***"
    newPos = currentGameState.getPacmanPosition()
    newFood = currentGameState.getFood()
    newPellets = currentGameState.getCapsules()
    newGhostStates = currentGameState.getGhostStates()
    newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]

    # get a the food and ghost positions
    newFood = newFood.asList()
    totalFood = len(newFood)
    gridGhostStates = currentGameState.getGhostPositions()
    # print newPellets

    foodScore = 0
    for i in newFood:
        # foodScore += 4/(util.manhattanDistance(newPos, i)+1)
        dist = util.manhattanDistance(newPos, i)
        if dist <= 3:
            foodScore += 32
        elif dist <= 6:
            foodScore += 16
        else:
            foodScore += 8
    # here we look we look at where the ghosts are
    # closer ghosts of course are bad and far away ones are ok but not great
    closestScore = 0
    for i in gridGhostStates:
        # closestScore += 6/(util.manhattanDistance(newPos, i)+1)
        dist = util.manhattanDistance(newPos, i)
        if dist <= 3:
            closestScore += 24
        elif dist <= 6:
            closestScore += 12
        else:
            closestScore += 2

    pelletScore = 0
    for i in newPellets:
        dist = util.manhattanDistance(newPos, i)
        if dist <= 1:
            pelletScore += 5
        else:
            pelletScore += 1

    score = currentGameState.getScore()

    if pelletScore != 0:
        score += (1.0 / float(pelletScore))
        score -= totalFood  # if we are going to find or wait for a pellet we need to be hurt by not chasing food

    if foodScore != 0:
        recip = (1.0 / float(foodScore))
        score += recip
        # score += -1/float(totalFood*15)

    if not all(x for x in
               newScaredTimes):  # if the ghosts are all white, then we dont need to worry about adding a ghost score
        score -= 1.0 / (float(closestScore) / 2.0)

    # print score
    return score


# Abbreviation
better = betterEvaluationFunction
