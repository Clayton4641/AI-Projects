# search.py
# ---------
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


"""
In search.py, you will implement generic search algorithms which are called by
Pacman agents (in searchAgents.py).
"""

import util

class SearchProblem:
    """
    This class outlines the structure of a search problem, but doesn't implement
    any of the methods (in object-oriented terminology: an abstract class).

    You do not need to change anything in this class, ever.
    """

    def getStartState(self):
        """
        Returns the start state for the search problem.
        """
        util.raiseNotDefined()

    def isGoalState(self, state):
        """
          state: Search state

        Returns True if and only if the state is a valid goal state.
        """
        util.raiseNotDefined()

    def getSuccessors(self, state):
        """
          state: Search state

        For a given state, this should return a list of triples, (successor,
        action, stepCost), where 'successor' is a successor to the current
        state, 'action' is the action required to get there, and 'stepCost' is
        the incremental cost of expanding to that successor.
        """
        util.raiseNotDefined()

    def getCostOfActions(self, actions):
        """
         actions: A list of actions to take

        This method returns the total cost of a particular sequence of actions.
        The sequence must be composed of legal moves.
        """
        util.raiseNotDefined()


def tinyMazeSearch(problem):
    """
    Returns a sequence of moves that solves tinyMaze.  For any other maze, the
    sequence of moves will be incorrect, so only use this for tinyMaze.
    """
    from game import Directions
    s = Directions.SOUTH
    w = Directions.WEST
    return  [s, s, w, s, w, w, s, w]

def depthFirstSearch(problem):
    """
    Search the deepest nodes in the search tree first.

    Your search algorithm needs to return a list of actions that reaches the
    goal. Make sure to implement a graph search algorithm.

    To get started, you might want to try some of these simple commands to
    understand the search problem that is being passed in:

    print "Start:", problem.getStartState()
    print "Is the start a goal?", problem.isGoalState(problem.getStartState())
    print "Start's successors:", problem.getSuccessors(problem.getStartState())
    """
    "*** YOUR CODE HERE ***"
    from game import Directions
    visitedNodes = []

    def dfsHelp(node,firstNode=False):

        if node[0] in visitedNodes: # if we have been here before
            return False, []

        visitedNodes.append(node[0])

        # this adds a move to the list if it isnt the first node
        moves = []
        if not firstNode:
            moves.append(node[1])

        sol = problem.isGoalState(node[0])

        if sol:
            return True, moves

        successors = problem.getSuccessors(node[0])[::-1]

        for i in successors:
            res, movs = dfsHelp(i)
            if res:
                return True, moves+movs

        return False, []

    state, directs = dfsHelp((problem.getStartState(),0,),True)
    # print len(directs)
    return directs

def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"
    visited = []

    moves = []

    q = util.Queue()
    q.push((problem.getStartState(),0,moves))
    visited.append(problem.getStartState())

    while not q.isEmpty(): # simple bfs
        node = q.pop()

        state = node[0]
        dirct = node[1]
        prevMoves = node[2]
        movs = [x for x in prevMoves]+[dirct]

        # print node

        if problem.isGoalState(state):
            # print movs[1:]
            return movs[1:]

        successors = problem.getSuccessors(state)
        # print 'bfs succ:',successors

        for i in successors:
            cords = i[0]
            dir = i[1]
            if cords not in visited:
                q.push((cords,dir,movs,))
                visited.append(cords)
    # print 'not path'
    return []

def bestfirstsearch(problem):
    node = (problem.getStartState(),'',0,[])

    frontier = util.PriorityQueue()
    frontier.update(node,node[2])

    reached = dict()
    reached[node[0]] = node

    while not frontier.isEmpty():
        node = frontier.pop()
        # print node
        if problem.isGoalState(node[0]):
            return node[3]
        for i in problem.getSuccessors(node[0]):
            pathcost = i[2]+node[2]
            if i[0] not in reached or pathcost<reached[i[0]][2]:
                # if i in reached and pathcost<reached[i[0]][2]:
                    # print pathcost,reached[i[0]][2]
                item = (i[0],i[1],pathcost,node[3]+[i[1]])
                reached[i[0]] = item
                frontier.push(item,item[2])
    return []

def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"
    return bestfirstsearch(problem)

def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0

def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"
    node = problem.getStartState()
    # print node
    node = (node, '', 0, [])

    openQ = util.PriorityQueue()
    openQ.update(node, 0)
    openS = []
    openS.append(node[0])

    closedList = []

    while not openQ.isEmpty():
        node = openQ.pop()
        # print 'node:',node
        # print problem.getSuccessors(node[0])
        if node[0] in closedList:
            continue
        if problem.isGoalState(node[0]):
            return node[3]

        openS.remove(node[0])
        closedList.append(node[0])

        for i in problem.getSuccessors(node[0]):
            # print 'i:',i
            path = node[3]+[i[1]]
            # print i[0]
            moves = problem.getCostOfActions(path)
            # print moves
            # print heuristic(i[0],problem)
            # print heuristic(i[0], problem)+moves
            openS.append(i[0])
            openQ.update((i[0],i[1],moves,path),moves+heuristic(i[0],problem))
    return []



# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch
