#include "pch.h"
#include "framework.h"
#include "Character.hpp"
#include "String.hpp" // needed for Character::ToString()
#include <cstddef>

namespace PumaType
{
	namespace
	{
		constexpr uint8_t UTF8CharSizeLookup[32] =
		{
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			1, 1, 1, 1, 1, 1, 1, 1,
			2, 2, 2, 2,
			3, 3,
			4,
			1
		};
	}

	Character::Character() noexcept
		: packedValue(0U)
	{
	}

	Character::Character(const Character& source) noexcept
		: packedValue(source.packedValue)
	{
	}

	Character::Character(const uint8_t* data) noexcept
	: packedValue(0U)
	{
		if (data == nullptr)
		{
			return;
		}

		const uint8_t charSize = GetCharSize(data[0]); // 1..4

		// Copy up to 4 bytes from the input data into the codeUnits array.
		memcpy_s(codeUnits, sizeof(codeUnits), data, charSize);
	}

	Character::~Character() noexcept = default;

	Character& Character::operator=(const Character& source) noexcept
	{
		if (this != &source)
		{
			packedValue = source.packedValue;
		}
		return *this;
	}

	String Character::ToString() const noexcept
	{
		const uint8_t charSize = GetCharSize(codeUnits[0]); // 1..4
		return String(codeUnits, charSize);
	}

	// Returns a pointer to the UTF-8 bytes of the string.
	// Do not delete the returned pointer; it is managed by the String class.
	// The returned pointer is valid as long as the String object is alive and not modified.
	// Return string that is not NUL-terminated; use Size() to determine the length in bytes.
	const uint8_t* Character::ToUTF8() const noexcept
	{
		return codeUnits;
	}

	const uint8_t Character::GetCharSize() const noexcept
	{
		return UTF8CharSizeLookup[codeUnits[0] >> 3];
	}

	const uint8_t Character::GetCharSize(const uint8_t c) noexcept
	{
		return UTF8CharSizeLookup[c >> 3];
	}
} // namespace PumaType