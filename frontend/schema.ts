/**
 * @file Contains schema for input and output
 */

/**
 * `0`: dead
 * `1`: alive
 */
export type Cell = 0 | 1;

/**
 * A page is made of `width * height` cells
 */
export type Page = Cell[];

/**
 * A simulation of con way game of life
 */
export type Story = Page[];
