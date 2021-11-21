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
 * Specifies how a page is laid out
 */
export interface Resolution {
  /**
   * How many cells are on the vertical axis.
   */
  width: number;

  /**
   * How many cells are laid out on the horizontal axis
   */
  height: number;
}

/**
 * A simulation of con way game of life
 */
export interface Story {
  /**
   * Resolution of the simulation
   */
  resolution: Resolution;

  /**
   * Iterations of the simulation
   */
  pages: Page[];
}
