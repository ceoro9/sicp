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

bool is_right_border(int n) {
	int row_number = find_row_number(n);

	return find_distance(n) == row_number - 1;
}

bool is_left_border(int n) {
	return find_distance(n) == 0;
}

void print_spaces_iter(int current_count, int total_count) {
	if (current_count > total_count) {
		return;
	}
	cout << " ";
	print_spaces_iter(current_count + 1, total_count);
}

void print_spaces(int count) {
	print_spaces_iter(0, count);
}

void print_number(int n, int total_rows_number) {
	int row_number = find_row_number(n);
	int value = find_number(n);

	if (n == 1) {
		print_spaces(total_rows_number - row_number);
		cout << find_number(n);
		print_spaces(total_rows_number - row_number);
		cout << "\n";
	} else if (is_left_border(n)) {
		print_spaces(total_rows_number - row_number);
		cout << value << " ";
	} else if (is_right_border(n)) {
		cout << value;
		print_spaces(total_rows_number - row_number);
		cout << "\n";
	} else {
		cout << value << " ";
	}
}

void print_pascal_triangle(int n, int total_rows_number) {
	if (n == 1) {
		print_number(1, total_rows_number);
		return;
	}
	print_pascal_triangle(n - 1, total_rows_number);
	print_number(n, total_rows_number);
}

void handle_pascal_triangle(int n) {
	int total_rows_number = find_row_number(n);
	print_pascal_triangle(n, total_rows_number);
}

int main()
{
	int n;

	cout << "n = "; cin >> n;
	
	cout << "\n\n";
	handle_pascal_triangle(n);
	cout << "\n\n";

	return 0;
}