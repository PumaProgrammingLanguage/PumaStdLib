#include "pch.h"
#include "framework.h"
#include "Charactor.hpp"
#include "String.hpp" // needed for Charactor::ToString()
#include <cstddef>

namespace Puma {
namespace Types
{
	namespace
	{
		constexpr std::uint8_t UTF8CharSizeLookup[32] =
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

	Charactor::Charactor() noexcept
		: packedValue(0U)
	{
	}

	Charactor::Charactor(const Charactor& source) noexcept
		: packedValue(source.packedValue)
	{
	}

	Charactor::Charactor(const char* data, std::size_t dataSize) noexcept
		: Charactor(reinterpret_cast<const std::uint8_t*>(data), static_cast<std::uint32_t>(dataSize))
	{
	}

	Charactor::Charactor(const std::uint8_t* data, std::uint32_t dataSize) noexcept
	: packedValue(0U)
	{
		if (data == nullptr || dataSize == 0)
		{
			return;
		}

		const std::uint8_t charSize = GetCharSize(data[0]); // 1..4

		if (charSize > dataSize)
		{
			// Invalid charSize, treat as empty character.
			return;
		}

		// Copy up to 4 bytes
		memcpy_s(codeUnits, sizeof(codeUnits), data, charSize);
	}

	Charactor::~Charactor() noexcept = default;

	Charactor& Charactor::operator=(const Charactor& source) noexcept
	{
		if (this != &source)
		{
			packedValue = source.packedValue;
		}
		return *this;
	}

	Types::String Charactor::ToString() const noexcept
	{
		const std::uint8_t charSize = GetCharSize(codeUnits[0]); // 1..4
		return String(codeUnits, charSize);
	}

	std::uint8_t Charactor::GetCharSize(const std::uint8_t c) noexcept
	{
		return UTF8CharSizeLookup[c >> 3];
	}
} // namespace Types
} // namespace Puma