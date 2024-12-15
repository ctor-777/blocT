#include <limits.h>
#include <stdlib.h>
#include <sys/time.h>
#include <stdio.h>
#include <time.h>

#ifndef N
# define N 10000
#endif
#ifndef I
# define I 10
#endif
#ifndef M
# define M 1000
#endif

typedef struct s_node {
	int data;
	struct s_node* next;
} node;

void insert_list(node** head, int data) {
	node *curr;
	// empty list
	if ((*head) == NULL) {
		(*head) = malloc(sizeof(node));
		(*head)->data = data;
		(*head)->next = NULL;
		return;
	}
	//insert at front
	if ((*head)->data > data) {
		node* new = malloc(sizeof(node));
		new->data = data;
		new->next = (*head);
		(*head) = new;
		return;
	}
	
	//anything else
	curr = *head;
	while (1) {
		if (curr->next == NULL || (curr->next->data > data && curr->data <= data) ) {

			node* new = malloc(sizeof(node));
			new->next = curr->next;
			new->data = data;

			curr->next = new;
			break;
		} else {
			curr = curr->next;
		}
	}
}

void insert_array(int** array, int* len, int data) {
	int* new_array = malloc(sizeof(int) * ((*len) + 1));
	int j = 0;

	//empty array
	if (len == 0) {
		new_array[0] = data;
		free(*array);
		(*len)++;
		return;
	}

	//anything else
	for (int i = 0; i <= *len; i++) {
		if (j < *len && data > (*array)[j]) {
			new_array[i] = (*array)[j];
			++j;
		} else {
			new_array[i] = data;
			data = INT_MAX;
		} 

	}
	
	free(*array);
	*array = new_array;
	(*len)++;	
}

void print_array(int* array, int len) {
	for(int i = 0; i < len; i++) {
		printf("%i ", array[i]);
	}
	printf("\n");
}

void print_list(node* head) {
	while(head != NULL) {
		printf("%i ", head->data);
		head = head->next;
	}
	printf("\n");
}

int test_list(int seed) {
	struct timeval start, end;
	node* head = NULL;

	srand(seed);
	for(int i = 0; i < N; i++) {
		int rand_num = (rand() % (2 * M)) - M;
		insert_list(&head, rand_num);
	}

	gettimeofday(&start, NULL);

	for(int i = 0; i < I; i++) {
		int rand_num = (rand() % (2 * M)) - M;
		insert_list(&head, rand_num);
	}

	gettimeofday(&end, NULL);

	/* printf("%i", end.tv_usec - start.tv_usec); */
	return end.tv_usec - start.tv_usec;
}

int test_array(int seed) {
	int* array = malloc(sizeof(int));
	int len = 0;
	struct timeval start, end;

	srand(seed);

	for(int i = 0; i < N; i++) {
		int rand_num = (rand() % (2 * M)) - M;
		insert_array(&array, &len, rand_num);
	}

	gettimeofday(&start, NULL);

	for(int i = 0; i < I; i++) {
		int rand_num = (rand() % (2 * M)) - M;
		insert_array(&array, &len, rand_num);
	}

	gettimeofday(&end, NULL);
	/* printf("%i", end.tv_usec - start.tv_usec); */
	return end.tv_usec - start.tv_usec;
}

int main(int argc, char** argv) {

	if (argc < 2)
		return 1;
	int* array = malloc(sizeof(int));
	node* head = NULL;
	int len = 0;
	struct timeval start, end;

	int time_list, time_array;
	int seed = time(NULL);

	if (atoi(argv[1]) == 0) {
		time_list = test_list(seed);
		time_array = test_array(seed);
	} else {
		time_array = test_array(seed);
		time_list = test_list(seed);
	}

	printf("%i, %i\n", time_array, time_list);


	return 0;
}
