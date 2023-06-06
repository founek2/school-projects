#include <iostream>
#include <queue>
using namespace std;

struct position{
    position(): x(0), y(0), steps(0){}
    position(int que, int pos, int number): x(que), y(pos), steps(number) {}
    int x;
    int y;
    int steps;
};


int main() {
    int queueLength, possibleSteps, wholePath = -1;

    cin >> queueLength >> possibleSteps;

    char** queue = new char*[2];
    queue[0] = new char[queueLength+1];
    queue[1] = new char[queueLength+1];

    cin >> queue[0];
    cin >> queue[1];


    std::queue<position> path;
    position defPosition;
    path.push(defPosition);

    while (!path.empty()){
        position opened = path.front();
        path.pop();

        // check for the end
        if(opened.y + possibleSteps >= queueLength || opened.y + 1 >= queueLength ){
            wholePath = opened.steps+1;
            break;
        }
        int secondQueue = (opened.x == 1 ? 0 : 1);

        // JUMP
        if(queue[secondQueue][opened.y+possibleSteps] != 'x'){
            position newPos(secondQueue, opened.y+possibleSteps, opened.steps + 1);
            path.push(newPos);
        }

        // Vpřed
        if(queue[opened.x][opened.y+1] != 'x'){
            position newPos(opened.x, opened.y + 1, opened.steps + 1);
            path.push(newPos);
        }
        if(opened.y - 1 >= 0){
            if(queue[opened.x][opened.y - 1] != 'x' && opened.y - 1 > opened.steps){ //TODO tweak
                position newPos(opened.x, opened.y - 1, opened.steps + 1);
                path.push(newPos);
            }
        }


        queue[opened.x][opened.y] = 'x';
    }

    cout << wholePath << endl;


    delete[] queue[0];
    delete[] queue[1];
    delete[] queue;
    return 0;
}