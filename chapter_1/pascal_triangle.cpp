#include <iostream>

using namespace std;

int find_row_number_iter(int start_row_number, int current_row_number, int x) {
	// if x lies between border numbers of current row -> return current_row_number;
	if (x >= start_row_number && x <= start_row_number + current_row_number - 1) {
		return current_row_number;
	}
	return find_row_number_iter(start_row_number + current_row_number, current_row_number + 1, x);
}

int find_row_number(int x) {
	return find_row_number_iter(1, 1, x);
}

int find_distance_iter(int start_row_number, int current_row_number, int x) {
	// if x lies between border number of current rows -> 
	// return distance between left border and current number
	if (x >= start_row_number && x <= start_row_number + current_row_number - 1) {
		return x - start_row_number;
	}
	return find_distance_iter(start_row_number + current_row_number, current_row_number + 1, x);	
}

int find_distance(int x) {
	return find_distance_iter(1, 1, x);
}

int find_number_by_row_number_and_distance_iter(int start_row_number, int current_row_number, int initial_row_number) {
	if (current_row_number == initial_row_number) {
		return start_row_number;
	}
	return find_number_by_row_number_and_distance_iter(start_row_number + current_row_number, current_row_number + 1, initial_row_number);
}

int find_number_by_row_number_and_distance(int row_number, int distance) {
	return find_number_by_row_number_and_distance_iter(1, 1, row_number) + distance;
}

int find_number(int n) {
	int current_row = find_row_number(n);
	int current_distance = find_distance(n);

	// if numbers lie on border -> 1
	if (current_distance == 0 || current_distance == current_row - 1) {
		return 1;
	} 

	int first_number = find_number_by_row_number_and_distance(current_row - 1, current_distance);
	int second_number = find_number_by_row_number_and_distance(current_row - 1, current_distance - 1);

	return find_number(first_number) + find_number(second_number);	
}

void print_pascal_triangle(int n) {
	if (n == 1) {
		cout << 1 << " ";
		return;
	}
	print_pascal_triangle(n - 1);
	cout << find_number(n) << " ";
}

int main()
{
	int n;

	cout << "n = "; cin >> n;
	print_pascal_triangle(n);

	return 0;
}