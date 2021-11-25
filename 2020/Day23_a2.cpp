#include <iostream>
using namespace std;

// #define VERBOSE

// #define TOTAL_LENGTH 9
// #define ITERATIONS 100

#define TOTAL_LENGTH 1000000
#define ITERATIONS 10000000

#define INITIAL_COUNT 9

struct ListItem
{
    int value;
    ListItem *next;
};

ListItem *CreateList(int initial[])
{
    ListItem *head = new ListItem();

    ListItem *prevItem = head;
    ListItem *nextItem;
    for (int i = TOTAL_LENGTH - 1; i >= INITIAL_COUNT; i--)
    {
        nextItem = new ListItem();
        nextItem->next = prevItem;
        nextItem->value = i + 1;

        prevItem = nextItem;
    }
    for (int i = INITIAL_COUNT - 1; i >= 1; i--)
    {
        nextItem = new ListItem();
        nextItem->next = prevItem;
        nextItem->value = initial[i];

        prevItem = nextItem;
    }

    head->value = initial[0];
    head->next = prevItem;

    return head;
}

#define PICKUP_COUNT 3

ListItem *move(ListItem *current)
{
    ListItem *pickup[PICKUP_COUNT];
    ListItem *next = current->next;
#ifdef VERBOSE
    cout << "pick up:";
#endif
    for (int i = 0; i < PICKUP_COUNT; i++)
    {
        pickup[i] = next;
#ifdef VERBOSE
        cout << " " << next->value;
#endif
        next = next->next;
    }
#ifdef VERBOSE
    cout << "\n";
#endif
    current->next = next; // cut out the three

    int dest_value = current->value - 1;
    for (int i = 0; i < PICKUP_COUNT; i++)
    {
        if (pickup[i]->value == dest_value)
        {
            dest_value -= 1;
            i = -1; // restart the loop
        }
        if (dest_value < 1)
        {
            dest_value = TOTAL_LENGTH;
            i = -1; // restart the loop
        }
    }
#ifdef VERBOSE
    cout << "destination: " << dest_value << "\n";
#endif
    while (next->value != dest_value)
    {
        next = next->next;
    }
    ListItem *dest = next;
    pickup[PICKUP_COUNT - 1]->next = dest->next;
    dest->next = pickup[0];

    return current->next;
}

void print_list(ListItem *head)
{
    cout << ":";
    for (int i = 0; i < TOTAL_LENGTH; i++)
    {
        cout << " " << head->value;
        head = head->next;
    }
    cout << "\n";
}

void print_shortvalues(ListItem *head)
{
    while (head->value != 1)
    {
        head = head->next;
    }
    head = head->next; // don't print 1 either
    for (int i = 1; i < TOTAL_LENGTH; i++)
    {
        cout << head->value;
        head = head->next;
    }
    cout << "\n";
}

int mult_after_1(ListItem *head)
{
    while (head->value != 1)
    {
        head = head->next;
    }
    head = head->next; // skip 1 itself
    return head->value * head->next->value;
}

int main()
{
    int initial[INITIAL_COUNT];

    cout << "> ";
    for (int i = 0; i < INITIAL_COUNT; i++)
    {
        char c;
        cin >> c;
        initial[i] = c - 0x30; // "1" -> 1, etc.
    }

    ListItem *current = CreateList(initial);
    // print_list(current);
    for (int i = 0; i < ITERATIONS; i++)
    {
        current = move(current);
        if (i * 10000 % ITERATIONS == 0)
        {
            cout << i << "\r";
            cout.flush();
        }
    }
    // print_shortvalues(current);
    cout << "\n" << mult_after_1(current) << "\n";
}
